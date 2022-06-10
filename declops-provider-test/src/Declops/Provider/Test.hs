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
import Test.QuickCheck hiding (output)
import Test.Syd
import Test.Syd.Validity hiding (check)
import Test.Syd.Validity.Aeson

providerJSONSpec ::
  forall input output.
  ( Show input,
    Eq input,
    GenValid input,
    Typeable input,
    FromJSON input,
    ToJSON input,
    HasCodec input,
    Show output,
    Eq output,
    GenValid output,
    Typeable output,
    FromJSON output,
    ToJSON output,
    HasCodec output
  ) =>
  Provider input output ->
  Spec
providerJSONSpec provider = do
  jsonSpec @input
  it "produces the same input schema as before" $
    let fp = concat ["test_resources/providers/", T.unpack (unProviderName (providerName provider)), "/schemas/input.txt"]
     in pureGoldenTextFile fp (TE.decodeUtf8 (renderColouredSchemaViaCodec @input))
  jsonSpec @output
  it "produces the same output schema as before" $
    let fp = concat ["test_resources/providers/", T.unpack (unProviderName (providerName provider)), "/schemas/output.txt"]
     in pureGoldenTextFile fp (TE.decodeUtf8 (renderColouredSchemaViaCodec @output))

localProviderSpec ::
  forall input output i.
  ( Show input,
    Eq input,
    Validity output,
    Show output,
    Eq output
  ) =>
  Bool ->
  Provider input output ->
  (i -> Gen input) ->
  SpecWith i
localProviderSpec debug provider genInput = do
  let name = providerName provider
  let evaluatingLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      evaluatingLog loc source level str = do
        _ <- evaluate loc
        _ <- evaluate source
        _ <- evaluate level
        _ <- evaluate str
        pure ()
  let runWithoutLogs func =
        if debug
          then runStderrLoggingT func
          else runLoggingT func evaluatingLog
  let query :: ResourceName -> IO (QueryResult output)
      query resourceName = runWithoutLogs $ runProviderQuery provider resourceName
  let apply :: ResourceName -> input -> IO (ApplyResult output)
      apply resourceName specification = runWithoutLogs $ runProviderApply provider resourceName specification
  let check :: ResourceName -> input -> IO (CheckResult output)
      check resourceName specification = runWithoutLogs $ runProviderCheck provider resourceName specification
  let destroy :: ResourceName -> IO DestroyResult
      destroy resourceName = runWithoutLogs $ runProviderDestroy provider resourceName
  let providerFail (ProviderException err) = expectationFailure err
  let requireQuerySuccess = \case
        QueryFailure err -> providerFail err
        QuerySuccess remoteState -> pure remoteState
  let requireApplySuccess = \case
        ApplyFailure err -> providerFail err
        ApplySuccess output -> pure output
  let requireCheckSuccess = \case
        CheckFailure err -> providerFail err
        CheckSuccess output -> pure output
  let requireDestroySuccess = \case
        DestroyFailure err -> providerFail err
        DestroySuccess -> pure ()

  describe (T.unpack $ unProviderName name) $ do
    describe "query" $ do
      it "is idempotent if the resource does not exist remotely" $ \_ ->
        forAllValid $ \resourceName -> do
          remoteState1 <- query resourceName
          remoteState2 <- query resourceName
          remoteState2 `shouldBe` remoteState1

    describe "apply" $ do
      it "can create a resource" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input
            output <- requireApplySuccess applyResult
            shouldBeValid output

      it "can create two of the same resources with different names" $ \i ->
        forAllValid $ \resourceName1 ->
          forAll (genValid `suchThat` (/= resourceName1)) $ \resourceName2 ->
            forAll (genInput i) $ \input -> do
              applyResult1 <- apply resourceName1 input
              _ <- requireApplySuccess applyResult1
              applyResult2 <- apply resourceName2 input
              _ <- requireApplySuccess applyResult2
              pure ()

      it "can query the resource that was just applied from scratch" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input
            output <- requireApplySuccess applyResult
            queryResult <- query resourceName
            remoteState <- requireQuerySuccess queryResult
            case remoteState of
              DoesNotExistRemotely -> expectationFailure "should have existed by now."
              ExistsRemotely output' -> output' `shouldBe` output

      it "can re-apply the resource that was just created and have the result be the same" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input
            output <- requireApplySuccess applyResult
            applyResult' <- apply resourceName input
            output' <- requireApplySuccess applyResult'
            output `shouldBe` output'

      it "is idempotent when the resource already exists and has not changed" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            -- Test starts here
            applyResult1 <- apply resourceName input
            applyResult2 <- apply resourceName input
            applyResult2 `shouldBe` applyResult1

      it "can apply a change" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i) $ \input2 -> do
              applyResult1 <- apply resourceName input1
              _ <- requireApplySuccess applyResult1
              applyResult2 <- apply resourceName input2
              _ <- requireApplySuccess applyResult2
              pure ()

      it "passes the check after applying" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            checkResult <- check resourceName input
            _ <- requireCheckSuccess checkResult
            pure ()

      it "fails the check after applying a different input" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i) $ \input2 -> do
              if input1 == input2
                then pure () -- Cannot run this test
                else do
                  applyResult <- apply resourceName input1
                  _ <- requireApplySuccess applyResult
                  checkResult <- check resourceName input2
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
              applyResult1 <- apply resourceName input1
              output1 <- requireApplySuccess applyResult1
              let ctxAfterFirst = unlines ["After first apply;", ppShow output1]
              context ctxAfterFirst $ do
                applyResult2 <- apply resourceName input2
                output2 <- requireApplySuccess applyResult2
                let ctxAfterSecond = unlines ["After second apply;", ppShow output2]
                context ctxAfterSecond $ do
                  checkResult <- check resourceName input2
                  _ <- requireCheckSuccess checkResult
                  pure ()

      it "can re-create a resource that exists locally but not remotely and pass a check" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            checkResult <- check resourceName input
            _ <- requireCheckSuccess checkResult
            pure ()

    describe "check" $ do
      it "is idempotent" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            -- Tests start here
            checkResult1 <- check resourceName input
            checkResult2 <- check resourceName input
            checkResult1 `shouldBe` checkResult2

      it "fails the check if nothing has been applied" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            checkResult <- check resourceName input
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
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            destroyResult <- destroy resourceName
            destroyResult `shouldBe` DestroySuccess
            checkResult <- check resourceName input
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
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            destroyResult <- destroy resourceName
            destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource was just created" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            -- Tests start here
            destroyResult1 <- destroy resourceName
            destroyResult2 <- destroy resourceName
            destroyResult1 `shouldBe` destroyResult2

      it "can no longer find the resource remotely after destroying it" $ \i ->
        forAllValid $ \resourceName ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- apply resourceName input
            _ <- requireApplySuccess applyResult
            -- Tests start here
            destroyResult <- destroy resourceName
            destroyResult `shouldBe` DestroySuccess

            queryResult <- query resourceName
            remoteState <- requireQuerySuccess queryResult
            remoteState `shouldBe` DoesNotExistRemotely

    it "can go through an entire cycle from nothing back to nothing" $ \i ->
      forAllValid $ \resourceName ->
        forAll (genInput i) $ \input -> do
          -- Apply
          applyResult <- apply resourceName input
          output <- requireApplySuccess applyResult

          -- Query
          queryResult <- query resourceName
          remoteState <- requireQuerySuccess queryResult
          case remoteState of
            DoesNotExistRemotely -> expectationFailure "should have existed remotely"
            ExistsRemotely output' -> output' `shouldBe` output

          -- Check
          checkResult <- check resourceName input
          _ <- requireCheckSuccess checkResult

          -- Destroy
          destroyResult <- destroy resourceName
          destroyResult `shouldBe` DestroySuccess

    it "can go through an entire cycle from nothing back to nothing, with a change" $ \i ->
      forAllValid $ \resourceName ->
        forAll (genInput i) $ \input1 -> do
          forAll (genInput i) $ \input2 -> do
            -- Apply
            applyResult1 <- apply resourceName input1
            output1 <- requireApplySuccess applyResult1
            let ctxAfterFirst = unlines ["After first apply;", ppShow output1]
            context ctxAfterFirst $ do
              -- Query
              queryResult1 <- query resourceName
              remoteState1 <- requireQuerySuccess queryResult1
              case remoteState1 of
                DoesNotExistRemotely -> expectationFailure "should have existed remotely"
                ExistsRemotely output -> output `shouldBe` output1

              -- Check
              checkResult1 <- check resourceName input1
              _ <- requireCheckSuccess checkResult1

              -- Change
              applyResult2 <- apply resourceName input2
              output2 <- requireApplySuccess applyResult2
              let ctxAfterSecond = unlines ["After second apply;", "output:", ppShow output2]
              context ctxAfterSecond $ do
                -- Query again
                queryResult2 <- query resourceName
                remoteState2 <- requireQuerySuccess queryResult2
                case remoteState2 of
                  DoesNotExistRemotely -> expectationFailure "should still have existed remotely"
                  ExistsRemotely output -> output `shouldBe` output2

                -- Check
                checkResult2 <- check resourceName input2
                _ <- requireCheckSuccess checkResult2

                -- Destroy
                destroyResult <- destroy resourceName
                requireDestroySuccess destroyResult
