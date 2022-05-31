{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Test where

import Autodocodec
import Autodocodec.Yaml
import Control.Exception
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Declops.Provider
import Declops.Provider.ProviderName
import Test.QuickCheck hiding (output)
import Test.Syd
import Test.Syd.Validity hiding (check)
import Test.Syd.Validity.Aeson

providerJSONSpec ::
  forall input reference output.
  ( Show input,
    Eq input,
    GenValid input,
    Typeable input,
    FromJSON input,
    ToJSON input,
    HasCodec input,
    Show reference,
    Eq reference,
    GenValid reference,
    Typeable reference,
    FromJSON reference,
    ToJSON reference,
    HasCodec reference,
    Show output,
    Eq output,
    GenValid output,
    Typeable output,
    FromJSON output,
    ToJSON output,
    HasCodec output
  ) =>
  Provider input reference output ->
  Spec
providerJSONSpec provider = do
  jsonSpec @input
  it "produces the same input schema as before" $
    let fp = concat ["test_resources/providers/", T.unpack (unProviderName (providerName provider)), "/schemas/input.txt"]
     in pureGoldenTextFile fp (TE.decodeUtf8 (renderColouredSchemaViaCodec @input))
  jsonSpec @reference
  it "produces the same reference schema as before" $
    let fp = concat ["test_resources/providers/", T.unpack (unProviderName (providerName provider)), "/schemas/reference.txt"]
     in pureGoldenTextFile fp (TE.decodeUtf8 (renderColouredSchemaViaCodec @reference))
  jsonSpec @output
  it "produces the same output schema as before" $
    let fp = concat ["test_resources/providers/", T.unpack (unProviderName (providerName provider)), "/schemas/output.txt"]
     in pureGoldenTextFile fp (TE.decodeUtf8 (renderColouredSchemaViaCodec @output))

localProviderSpec ::
  forall input reference output i.
  ( Show input,
    Eq input,
    Validity reference,
    Show reference,
    Eq reference,
    Validity output,
    Show output,
    Eq output
  ) =>
  Provider input reference output ->
  (i -> Gen reference) ->
  (i -> Gen input) ->
  SpecWith i
localProviderSpec provider genReference genInput = do
  let name = providerName provider
  let evaluatingLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      evaluatingLog loc source level str = do
        _ <- evaluate loc
        _ <- evaluate source
        _ <- evaluate level
        _ <- evaluate str
        pure ()
  let runWithoutLogs func = runLoggingT func evaluatingLog
  let query reference = runWithoutLogs $ runProviderQuery provider reference
  let apply specification applyContext = runWithoutLogs $ runProviderApply provider specification applyContext
  let check specification reference = runWithoutLogs $ runProviderCheck provider specification reference
  let destroy reference = runWithoutLogs $ runProviderDestroy provider reference
  let providerFail (ProviderException err) = expectationFailure err
  let requireQuerySuccess = \case
        QueryFailure err -> providerFail err
        QuerySuccess remoteState -> pure remoteState
  let requireApplySuccess = \case
        ApplyFailure err -> providerFail err
        ApplySuccess reference output -> pure (reference, output)
  let requireCheckSuccess = \case
        CheckFailure err -> providerFail err
        CheckSuccess output -> pure output
  let requireDestroySuccess = \case
        DestroyFailure err -> providerFail err
        DestroySuccess -> pure ()

  describe (T.unpack $ unProviderName name) $ do
    describe "query" $ do
      it "is idempotent if the resource does not exist remotely" $ \i ->
        forAll (genReference i) $ \reference -> do
          remoteState1 <- query reference
          remoteState2 <- query reference
          remoteState2 `shouldBe` remoteState1

    describe "apply" $ do
      it "can create a resource if there is no local state" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, output) <- requireApplySuccess applyResult
          shouldBeValid reference
          shouldBeValid output

      it "can create two of the same resources and have them be different" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult1 <- apply input DoesNotExistLocallyNorRemotely
          (reference1, _) <- requireApplySuccess applyResult1
          applyResult2 <- apply input DoesNotExistLocallyNorRemotely
          (reference2, _) <- requireApplySuccess applyResult2
          reference1 `shouldNotBe` reference2

      it "can query the resource that was just applied from scratch" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, output) <- requireApplySuccess applyResult
          queryResult <- query reference
          remoteState <- requireQuerySuccess queryResult
          case remoteState of
            DoesNotExistRemotely -> expectationFailure "should have existed by now."
            ExistsRemotely output' -> output' `shouldBe` output

      it "can re-apply the resource that was just created and have the result be the same" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, output) <- requireApplySuccess applyResult
          applyResult' <- apply input (ExistsLocallyAndRemotely reference output)
          applyResult' `shouldBe` applyResult

      it "is idempotent when the resource already exists and has not changed" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, output) <- requireApplySuccess applyResult
          -- Test starts here
          applyResult1 <- apply input (ExistsLocallyAndRemotely reference output)
          applyResult2 <- apply input (ExistsLocallyAndRemotely reference output)
          applyResult2 `shouldBe` applyResult1

      it "can apply a change" $ \i ->
        forAll (genInput i) $ \input1 ->
          forAll (genInput i) $ \input2 -> do
            applyResult1 <- apply input1 DoesNotExistLocallyNorRemotely
            (reference1, output1) <- requireApplySuccess applyResult1
            applyResult2 <- apply input2 (ExistsLocallyAndRemotely reference1 output1)
            (_, _) <- requireApplySuccess applyResult2
            pure ()

      it "can re-create a resource that exists locally but not remotely" $ \i ->
        forAll (genReference i) $ \reference1 ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply input (ExistsLocallyButNotRemotely reference1)
            (_, _) <- requireApplySuccess applyResult
            pure ()

      it "passes the check after applying" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, _) <- requireApplySuccess applyResult
          checkResult <- check input reference
          _ <- requireCheckSuccess checkResult
          pure ()

      it "fails the check after applying a different input" $ \i ->
        forAll (genInput i) $ \input1 ->
          forAll (genInput i `suchThat` (/= input1)) $ \input2 -> do
            applyResult <- apply input1 DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            checkResult <- check input2 reference
            case checkResult of
              CheckFailure _ -> pure ()
              CheckSuccess output ->
                liftIO $
                  expectationFailure $
                    unlines
                      [ "should not have succeeded, but did and got this output:",
                        ppShow output
                      ]

      it "can apply a change and pass a check" $ \i ->
        forAll (genInput i) $ \input1 ->
          forAll (genInput i) $ \input2 -> do
            applyResult1 <- apply input1 DoesNotExistLocallyNorRemotely
            (reference1, output1) <- requireApplySuccess applyResult1
            let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
            context ctxAfterFirst $ do
              applyResult2 <- apply input2 (ExistsLocallyAndRemotely reference1 output1)
              (reference2, output2) <- requireApplySuccess applyResult2
              let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
              context ctxAfterSecond $ do
                checkResult <- check input2 reference2
                _ <- requireCheckSuccess checkResult
                pure ()

      it "can re-create a resource that exists locally but not remotely and pass a check" $ \i ->
        forAll (genReference i) $ \reference1 ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply input (ExistsLocallyButNotRemotely reference1)
            (reference, _) <- requireApplySuccess applyResult
            checkResult <- check input reference
            _ <- requireCheckSuccess checkResult
            pure ()

    describe "check" $ do
      it "is idempotent" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, _) <- requireApplySuccess applyResult
          -- Tests start here
          checkResult1 <- check input reference
          checkResult2 <- check input reference
          checkResult1 `shouldBe` checkResult2

      it "fails the check if nothing has been applied" $ \i ->
        forAll (genInput i) $ \input -> do
          forAll (genReference i) $ \reference -> do
            checkResult <- check input reference
            case checkResult of
              CheckFailure _ -> pure ()
              CheckSuccess output ->
                liftIO $
                  expectationFailure $
                    unlines
                      [ "should not have succeeded, but did and got this output:",
                        ppShow output
                      ]

      it "fails the check after a destroy" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, _) <- requireApplySuccess applyResult
          destroyResult <- destroy reference
          destroyResult `shouldBe` DestroySuccess
          checkResult <- check input reference
          case checkResult of
            CheckFailure _ -> pure ()
            CheckSuccess output ->
              liftIO $
                expectationFailure $
                  unlines
                    [ "should not have succeeded, but did and got this output:",
                      ppShow output
                    ]

    describe "destroy" $ do
      it "can destroy a resource that was just created" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, _) <- requireApplySuccess applyResult
          destroyResult <- destroy reference
          destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource was just created" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, _) <- requireApplySuccess applyResult
          -- Tests start here
          destroyResult1 <- destroy reference
          destroyResult2 <- destroy reference
          destroyResult1 `shouldBe` destroyResult2

      it "can destroy a resource that exists locally but not remotely" $ \i ->
        forAll (genReference i) $ \reference -> do
          destroyResult <- destroy reference
          destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource existed locally but not remotely" $ \i ->
        forAll (genReference i) $ \reference -> do
          destroyResult1 <- destroy reference
          destroyResult2 <- destroy reference
          destroyResult1 `shouldBe` destroyResult2

      it "can no longer find the resource remotely after destroying it" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- apply input DoesNotExistLocallyNorRemotely
          (reference, _) <- requireApplySuccess applyResult
          -- Tests start here
          destroyResult <- destroy reference
          destroyResult `shouldBe` DestroySuccess

          queryResult <- query reference
          remoteState <- requireQuerySuccess queryResult
          remoteState `shouldBe` DoesNotExistRemotely

    it "can go through an entire cycle from nothing back to nothing" $ \i ->
      forAll (genInput i) $ \input -> do
        -- Apply
        applyResult <- apply input DoesNotExistLocallyNorRemotely
        (reference, output) <- requireApplySuccess applyResult
        -- Query
        queryResult <- query reference
        remoteState <- requireQuerySuccess queryResult
        case remoteState of
          DoesNotExistRemotely -> expectationFailure "should have existed remotely"
          ExistsRemotely output' -> output' `shouldBe` output

        -- Check
        checkResult <- check input reference
        _ <- requireCheckSuccess checkResult

        -- Destroy
        destroyResult <- destroy reference
        destroyResult `shouldBe` DestroySuccess

    it "can go through an entire cycle from nothing back to nothing, with a change" $ \i ->
      forAll (genInput i) $ \input1 -> do
        forAll (genInput i) $ \input2 -> do
          -- Apply
          applyResult1 <- apply input1 DoesNotExistLocallyNorRemotely
          (reference1, output1) <- requireApplySuccess applyResult1
          let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
          context ctxAfterFirst $ do
            -- Query
            queryResult1 <- query reference1
            remoteState1 <- requireQuerySuccess queryResult1
            case remoteState1 of
              DoesNotExistRemotely -> expectationFailure "should have existed remotely"
              ExistsRemotely output -> output `shouldBe` output1

            -- Check
            checkResult1 <- check input1 reference1
            _ <- requireCheckSuccess checkResult1

            -- Change
            applyResult2 <- apply input2 (ExistsLocallyAndRemotely reference1 output1)
            (reference2, output2) <- requireApplySuccess applyResult2
            let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
            context ctxAfterSecond $ do
              -- Query again
              queryResult2 <- query reference2
              remoteState2 <- requireQuerySuccess queryResult2
              case remoteState2 of
                DoesNotExistRemotely -> expectationFailure "should still have existed remotely"
                ExistsRemotely output -> output `shouldBe` output2

              -- Check
              checkResult2 <- check input2 reference2
              _ <- requireCheckSuccess checkResult2

              -- Destroy
              destroyResult <- destroy reference2
              requireDestroySuccess destroyResult
