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
import Declops.Provider.Gen ()
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
  let query resourceName reference = runWithoutLogs $ runProviderQuery provider resourceName reference
  let apply resourceName specification applyContext = runWithoutLogs $ runProviderApply provider resourceName specification applyContext
  let check resourceName specification reference = runWithoutLogs $ runProviderCheck provider resourceName specification reference
  let destroy resourceName reference = runWithoutLogs $ runProviderDestroy provider resourceName reference
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
        forAllValid $ \resourceName ->
          forAll (genReference i) $ \reference -> do
            remoteState1 <- query resourceName reference
            remoteState2 <- query resourceName reference
            remoteState2 `shouldBe` remoteState1

    describe "apply" $ do
      it "can create a resource if there is no local state" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, output) <- requireApplySuccess applyResult
            shouldBeValid reference
            shouldBeValid output

      it "can create two of the same resources and have them be different" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult1 <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference1, _) <- requireApplySuccess applyResult1
            applyResult2 <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference2, _) <- requireApplySuccess applyResult2
            reference1 `shouldNotBe` reference2

      it "can query the resource that was just applied from scratch" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, output) <- requireApplySuccess applyResult
            queryResult <- query resourceName reference
            remoteState <- requireQuerySuccess queryResult
            case remoteState of
              DoesNotExistRemotely -> expectationFailure "should have existed by now."
              ExistsRemotely output' -> output' `shouldBe` output

      it "can re-apply the resource that was just created and have the result be the same" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, output) <- requireApplySuccess applyResult
            applyResult' <- apply resourceName input (ExistsLocallyAndRemotely reference output)
            applyResult' `shouldBe` applyResult

      it "is idempotent when the resource already exists and has not changed" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, output) <- requireApplySuccess applyResult
            -- Test starts here
            applyResult1 <- apply resourceName input (ExistsLocallyAndRemotely reference output)
            applyResult2 <- apply resourceName input (ExistsLocallyAndRemotely reference output)
            applyResult2 `shouldBe` applyResult1

      it "can apply a change" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i) $ \input2 -> do
              applyResult1 <- apply resourceName input1 DoesNotExistLocallyNorRemotely
              (reference1, output1) <- requireApplySuccess applyResult1
              applyResult2 <- apply resourceName input2 (ExistsLocallyAndRemotely reference1 output1)
              (_, _) <- requireApplySuccess applyResult2
              pure ()

      it "can re-create a resource that exists locally but not remotely" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genReference i) $ \reference1 ->
            forAll (genInput i) $ \input -> do
              applyResult <- apply resourceName input (ExistsLocallyButNotRemotely reference1)
              (_, _) <- requireApplySuccess applyResult
              pure ()

      it "passes the check after applying" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            checkResult <- check resourceName input reference
            _ <- requireCheckSuccess checkResult
            pure ()

      it "fails the check after applying a different input" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i `suchThat` (/= input1)) $ \input2 -> do
              applyResult <- apply resourceName input1 DoesNotExistLocallyNorRemotely
              (reference, _) <- requireApplySuccess applyResult
              checkResult <- check resourceName input2 reference
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
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i) $ \input2 -> do
              applyResult1 <- apply resourceName input1 DoesNotExistLocallyNorRemotely
              (reference1, output1) <- requireApplySuccess applyResult1
              let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
              context ctxAfterFirst $ do
                applyResult2 <- apply resourceName input2 (ExistsLocallyAndRemotely reference1 output1)
                (reference2, output2) <- requireApplySuccess applyResult2
                let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
                context ctxAfterSecond $ do
                  checkResult <- check resourceName input2 reference2
                  _ <- requireCheckSuccess checkResult
                  pure ()

      it "can re-create a resource that exists locally but not remotely and pass a check" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genReference i) $ \reference1 ->
            forAll (genInput i) $ \input -> do
              applyResult <- apply resourceName input (ExistsLocallyButNotRemotely reference1)
              (reference, _) <- requireApplySuccess applyResult
              checkResult <- check resourceName input reference
              _ <- requireCheckSuccess checkResult
              pure ()

    describe "check" $ do
      it "is idempotent" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            -- Tests start here
            checkResult1 <- check resourceName input reference
            checkResult2 <- check resourceName input reference
            checkResult1 `shouldBe` checkResult2

      it "fails the check resourceName if nothing has been applied" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            forAll (genReference i) $ \reference -> do
              checkResult <- check resourceName input reference
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
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            destroyResult <- destroy resourceName reference
            destroyResult `shouldBe` DestroySuccess
            checkResult <- check resourceName input reference
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
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            destroyResult <- destroy resourceName reference
            destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource was just created" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            -- Tests start here
            destroyResult1 <- destroy resourceName reference
            destroyResult2 <- destroy resourceName reference
            destroyResult1 `shouldBe` destroyResult2

      it "can destroy a resource that exists locally but not remotely" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genReference i) $ \reference -> do
            destroyResult <- destroy resourceName reference
            destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource existed locally but not remotely" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genReference i) $ \reference -> do
            destroyResult1 <- destroy resourceName reference
            destroyResult2 <- destroy resourceName reference
            destroyResult1 `shouldBe` destroyResult2

      it "can no longer find the resource remotely after destroying it" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
            (reference, _) <- requireApplySuccess applyResult
            -- Tests start here
            destroyResult <- destroy resourceName reference
            destroyResult `shouldBe` DestroySuccess

            queryResult <- query resourceName reference
            remoteState <- requireQuerySuccess queryResult
            remoteState `shouldBe` DoesNotExistRemotely

    it "can go through an entire cycle from nothing back to nothing" $ \i ->
      forAllValid $ \resourceName ->
        forAll (genInput i) $ \input -> do
          -- Apply
          applyResult <- apply resourceName input DoesNotExistLocallyNorRemotely
          (reference, output) <- requireApplySuccess applyResult
          -- Query
          queryResult <- query resourceName reference
          remoteState <- requireQuerySuccess queryResult
          case remoteState of
            DoesNotExistRemotely -> expectationFailure "should have existed remotely"
            ExistsRemotely output' -> output' `shouldBe` output

          -- Check
          checkResult <- check resourceName input reference
          _ <- requireCheckSuccess checkResult

          -- Destroy
          destroyResult <- destroy resourceName reference
          destroyResult `shouldBe` DestroySuccess

    it "can go through an entire cycle from nothing back to nothing, with a change" $ \i ->
      forAllValid $ \resourceName ->
        forAll (genInput i) $ \input1 -> do
          forAll (genInput i) $ \input2 -> do
            -- Apply
            applyResult1 <- apply resourceName input1 DoesNotExistLocallyNorRemotely
            (reference1, output1) <- requireApplySuccess applyResult1
            let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
            context ctxAfterFirst $ do
              -- Query
              queryResult1 <- query resourceName reference1
              remoteState1 <- requireQuerySuccess queryResult1
              case remoteState1 of
                DoesNotExistRemotely -> expectationFailure "should have existed remotely"
                ExistsRemotely output -> output `shouldBe` output1

              -- Check
              checkResult1 <- check resourceName input1 reference1
              _ <- requireCheckSuccess checkResult1

              -- Change
              applyResult2 <- apply resourceName input2 (ExistsLocallyAndRemotely reference1 output1)
              (reference2, output2) <- requireApplySuccess applyResult2
              let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
              context ctxAfterSecond $ do
                -- Query again
                queryResult2 <- query resourceName reference2
                remoteState2 <- requireQuerySuccess queryResult2
                case remoteState2 of
                  DoesNotExistRemotely -> expectationFailure "should still have existed remotely"
                  ExistsRemotely output -> output `shouldBe` output2

                -- Check
                checkResult2 <- check resourceName input2 reference2
                _ <- requireCheckSuccess checkResult2

                -- Destroy
                destroyResult <- destroy resourceName reference2
                requireDestroySuccess destroyResult
