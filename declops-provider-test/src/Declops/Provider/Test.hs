{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Test where

import Autodocodec
import Autodocodec.Yaml
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Declops.DB
import Declops.Provider
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
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
localProviderSpec Provider {..} genReference genInput = do
  modifyMaxSuccess (`div` 10) $
    describe (T.unpack $ unProviderName providerName) $ do
      describe "query" $ do
        it "is idempotent if the resource does not exist remotely" $ \i ->
          forAll (genReference i) $ \reference -> do
            remoteState1 <- providerQuery reference
            remoteState2 <- providerQuery reference
            remoteState2 `shouldBe` remoteState1

      describe "apply" $ do
        it "can create a resource if there is no local state" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference output -> do
                shouldBeValid reference
                shouldBeValid output

        it "can create two of the same resources and have them be different" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult1 <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult1 of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference1 _ -> do
                applyResult2 <- providerApply input DoesNotExistLocallyNorRemotely
                case applyResult2 of
                  ApplyFailure err -> expectationFailure err
                  ApplySuccess reference2 _ -> do
                    reference1 `shouldNotBe` reference2

        it "can query the resource that was just applied from scratch" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference output -> do
                remoteState <- providerQuery reference
                case remoteState of
                  DoesNotExistRemotely -> expectationFailure "should have existed by now."
                  ExistsRemotely output' -> output' `shouldBe` output

        it "can re-apply the resource that was just created and have the result be the same" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference output -> do
                applyResult' <- providerApply input (ExistsLocallyAndRemotely reference output)
                applyResult' `shouldBe` applyResult

        it "is idempotent when the resource already exists and has not changed" $ \i ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference output -> do
                -- Test starts here
                applyResult1 <- providerApply input (ExistsLocallyAndRemotely reference output)
                applyResult2 <- providerApply input (ExistsLocallyAndRemotely reference output)
                applyResult2 `shouldBe` applyResult1

        it "can apply a change" $ \i ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i) $ \input2 -> do
              applyResult1 <- providerApply input1 DoesNotExistLocallyNorRemotely
              case applyResult1 of
                ApplyFailure err -> expectationFailure err
                ApplySuccess reference1 output1 -> do
                  applyResult2 <- providerApply input2 (ExistsLocallyAndRemotely reference1 output1)
                  case applyResult2 of
                    ApplyFailure err -> expectationFailure err
                    ApplySuccess _ _ -> pure ()

        it "can re-create a resource that exists locally but not remotely" $ \i ->
          forAll (genReference i) $ \reference1 ->
            forAll (genInput i) $ \input -> do
              applyResult <- providerApply input (ExistsLocallyButNotRemotely reference1)
              case applyResult of
                ApplyFailure err -> expectationFailure err
                ApplySuccess _ _ -> pure ()

        it "passes the check after applying" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference _ -> do
                checkResult <- providerCheck input reference
                case checkResult of
                  CheckFailure err -> expectationFailure err
                  CheckSuccess _ -> pure ()

        it "fails the check after applying a different input" $ \i ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i `suchThat` (/= input1)) $ \input2 -> do
              applyResult <- providerApply input1 DoesNotExistLocallyNorRemotely
              case applyResult of
                ApplyFailure err -> expectationFailure err
                ApplySuccess reference _ -> do
                  checkResult <- providerCheck input2 reference
                  case checkResult of
                    CheckFailure _ -> pure ()
                    CheckSuccess output ->
                      expectationFailure $
                        unlines
                          [ "should not have succeeded, but did and got this output:",
                            ppShow output
                          ]

        it "can apply a change and pass a check" $ \i ->
          forAll (genInput i) $ \input1 ->
            forAll (genInput i) $ \input2 -> do
              applyResult1 <- providerApply input1 DoesNotExistLocallyNorRemotely
              case applyResult1 of
                ApplyFailure err -> expectationFailure err
                ApplySuccess reference1 output1 ->
                  let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
                   in context ctxAfterFirst $ do
                        applyResult2 <- providerApply input2 (ExistsLocallyAndRemotely reference1 output1)
                        case applyResult2 of
                          ApplyFailure err -> expectationFailure err
                          ApplySuccess reference2 output2 ->
                            let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
                             in context ctxAfterSecond $ do
                                  checkResult <- providerCheck input2 reference2
                                  case checkResult of
                                    CheckFailure err -> expectationFailure err
                                    CheckSuccess _ -> pure ()

        it "can re-create a resource that exists locally but not remotely and pass a check" $ \i ->
          forAll (genReference i) $ \reference1 ->
            forAll (genInput i) $ \input -> do
              applyResult <- providerApply input (ExistsLocallyButNotRemotely reference1)
              case applyResult of
                ApplyFailure err -> expectationFailure err
                ApplySuccess reference _ -> do
                  checkResult <- providerCheck input reference
                  case checkResult of
                    CheckFailure err -> expectationFailure err
                    CheckSuccess _ -> pure ()

      describe "check" $ do
        it "is idempotent" $ \i ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference _ -> do
                -- Tests start here
                checkResult1 <- providerCheck input reference
                checkResult2 <- providerCheck input reference
                checkResult1 `shouldBe` checkResult2

        it "fails the check if nothing has been applied" $ \i ->
          forAll (genInput i) $ \input -> do
            forAll (genReference i) $ \reference -> do
              checkResult <- providerCheck input reference
              case checkResult of
                CheckFailure _ -> pure ()
                CheckSuccess output ->
                  expectationFailure $
                    unlines
                      [ "should not have succeeded, but did and got this output:",
                        ppShow output
                      ]

        it "fails the check after a destroy" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference _ -> do
                destroyResult <- providerDestroy reference
                destroyResult `shouldBe` DestroySuccess

                checkResult <- providerCheck input reference
                case checkResult of
                  CheckFailure _ -> pure ()
                  CheckSuccess output ->
                    expectationFailure $
                      unlines
                        [ "should not have succeeded, but did and got this output:",
                          ppShow output
                        ]

      describe "destroy" $ do
        it "can destroy a resource that was just created" $ \i ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference _ -> do
                destroyResult <- providerDestroy reference
                destroyResult `shouldBe` DestroySuccess

        it "is idempotent when the resource was just created" $ \i ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference _ -> do
                -- Tests start here
                destroyResult1 <- providerDestroy reference
                destroyResult2 <- providerDestroy reference
                destroyResult1 `shouldBe` destroyResult2

        it "can destroy a resource that exists locally but not remotely" $ \i ->
          forAll (genReference i) $ \reference -> do
            destroyResult <- providerDestroy reference
            destroyResult `shouldBe` DestroySuccess

        it "is idempotent when the resource existed locally but not remotely" $ \i ->
          forAll (genReference i) $ \reference -> do
            destroyResult1 <- providerDestroy reference
            destroyResult2 <- providerDestroy reference
            destroyResult1 `shouldBe` destroyResult2

        it "can no longer find the resource remotely after destroying it" $ \i ->
          forAll (genInput i) $ \input -> do
            -- Setup
            applyResult <- providerApply input DoesNotExistLocallyNorRemotely
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference _ -> do
                -- Tests start here
                destroyResult <- providerDestroy reference
                destroyResult `shouldBe` DestroySuccess

                remoteState <- providerQuery reference
                remoteState `shouldBe` DoesNotExistRemotely

      it "can go through an entire cycle from nothing back to nothing" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Apply
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              -- Query
              remoteState <- providerQuery reference
              case remoteState of
                DoesNotExistRemotely -> expectationFailure "should have eisted remotely"
                ExistsRemotely output' -> output' `shouldBe` output

              -- Check
              checkResult <- providerCheck input reference
              case checkResult of
                CheckSuccess _ -> pure ()
                CheckFailure err -> expectationFailure err

              -- Destroy
              destroyResult <- providerDestroy reference
              destroyResult `shouldBe` DestroySuccess

      it "can go through an entire cycle from nothing back to nothing, with a change" $ \i ->
        forAll (genInput i) $ \input1 -> do
          forAll (genInput i) $ \input2 -> do
            -- Apply
            applyResult1 <- providerApply input1 DoesNotExistLocallyNorRemotely
            case applyResult1 of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference1 output1 ->
                let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
                 in context ctxAfterFirst $ do
                      -- Query
                      remoteState1 <- providerQuery reference1
                      case remoteState1 of
                        DoesNotExistRemotely -> expectationFailure "should have existed remotely"
                        ExistsRemotely output -> output `shouldBe` output1

                      -- Check
                      checkResult1 <- providerCheck input1 reference1
                      case checkResult1 of
                        CheckFailure err -> expectationFailure err
                        CheckSuccess _ -> pure ()

                      -- Change
                      applyResult2 <- providerApply input2 (ExistsLocallyAndRemotely reference1 output1)
                      case applyResult2 of
                        ApplyFailure err -> expectationFailure err
                        ApplySuccess reference2 output2 ->
                          let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
                           in context ctxAfterSecond $ do
                                -- Query again
                                remoteState2 <- providerQuery reference2
                                case remoteState2 of
                                  DoesNotExistRemotely -> expectationFailure "should still have existed remotely"
                                  ExistsRemotely output -> output `shouldBe` output2

                                -- Check
                                checkResult2 <- providerCheck input2 reference2
                                case checkResult2 of
                                  CheckFailure err -> expectationFailure err
                                  CheckSuccess _ -> pure ()

                                -- Destroy
                                destroyResult <- providerDestroy reference2
                                destroyResult `shouldBe` DestroySuccess