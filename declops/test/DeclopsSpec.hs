{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DeclopsSpec (spec) where

import Data.GenValidity.Path ()
import Data.Proxy
import qualified Data.Text as T
import Declops
import Path
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "tempDirProvider" $
    tempDirSpec "declops-temp-dir-provider-test" $
      localProviderSpec
        tempDirProvider
        (\tdir -> ((</>) <$> pure tdir <*> genValid))
        (\tdir -> TempDirInputs tdir <$> elements ["foo", "bar", "quux"])
  pure ()

localProviderSpec ::
  forall input reference output i.
  ( Show input,
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
localProviderSpec Provider {..} genReference genInput = modifyMaxSuccess (`div` 50) $
  describe (T.unpack providerName) $ do
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
                  ApplySuccess reference2 output2 -> pure ()

      it "can re-create a resource that exists locally but not remotely" $ \i ->
        forAll (genReference i) $ \reference1 ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input (ExistsLocallyButNotRemotely reference1)
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference2 output -> pure ()

      it "passes the check after applying" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              checkResult <- providerCheck input output
              case checkResult of
                CheckFailure err -> expectationFailure err
                CheckSuccess -> pure ()

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
                                checkResult <- providerCheck input2 output2
                                case checkResult of
                                  CheckFailure err -> expectationFailure err
                                  CheckSuccess -> pure ()

      it "can re-create a resource that exists locally but not remotely and pass a check" $ \i ->
        forAll (genReference i) $ \reference1 ->
          forAll (genInput i) $ \input -> do
            applyResult <- providerApply input (ExistsLocallyButNotRemotely reference1)
            case applyResult of
              ApplyFailure err -> expectationFailure err
              ApplySuccess reference2 output -> do
                checkResult <- providerCheck input output
                case checkResult of
                  CheckFailure err -> expectationFailure err
                  CheckSuccess -> pure ()

    describe "check" $ do
      it "is idempotent" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              -- Tests start here
              checkResult1 <- providerCheck input output
              checkResult2 <- providerCheck input output
              checkResult1 `shouldBe` checkResult2

    describe "destroy" $ do
      it "can destroy a resource that was just created" $ \i ->
        forAll (genInput i) $ \input -> do
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              destroyResult <- providerDestroy reference (ExistsRemotely output)
              destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource was just created" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              -- Tests start here
              destroyResult1 <- providerDestroy reference (ExistsRemotely output)
              destroyResult2 <- providerDestroy reference DoesNotExistRemotely
              destroyResult1 `shouldBe` destroyResult2

      it "is idempotent when the resource was just created, even if we thought the resource still existed" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              -- Tests start here
              destroyResult1 <- providerDestroy reference (ExistsRemotely output)
              destroyResult2 <- providerDestroy reference (ExistsRemotely output)
              destroyResult1 `shouldBe` destroyResult2

      it "can destroy a resource that exists locally but not remotely" $ \i ->
        forAll (genReference i) $ \reference -> do
          destroyResult <- providerDestroy reference DoesNotExistRemotely
          destroyResult `shouldBe` DestroySuccess

      it "is idempotent when the resource existed locally but not remotely" $ \i ->
        forAll (genReference i) $ \reference -> do
          destroyResult1 <- providerDestroy reference DoesNotExistRemotely
          destroyResult2 <- providerDestroy reference DoesNotExistRemotely
          destroyResult1 `shouldBe` destroyResult2

      it "can no longer find the resource remotely after destroying it" $ \i ->
        forAll (genInput i) $ \input -> do
          -- Setup
          applyResult <- providerApply input DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference output -> do
              -- Tests start here
              destroyResult <- providerDestroy reference (ExistsRemotely output)
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
            remoteState `shouldBe` ExistsRemotely output

            -- Check
            checkResult <- providerCheck input output
            case checkResult of
              CheckFailure err -> expectationFailure err
              CheckSuccess -> pure ()

            -- Destroy
            destroyResult <- providerDestroy reference (ExistsRemotely output)
            destroyResult `shouldBe` DestroySuccess

    it "can go through an entire cycle from nothing back to nothing, with a change" $ \i ->
      forAll (genInput i) $ \input1 -> do
        forAll (genInput i) $ \input2 -> do
          -- Apply
          applyResult <- providerApply input1 DoesNotExistLocallyNorRemotely
          case applyResult of
            ApplyFailure err -> expectationFailure err
            ApplySuccess reference1 output1 ->
              let ctxAfterFirst = unlines ["After first apply;", "reference:", ppShow reference1, "output:", ppShow output1]
               in context ctxAfterFirst $ do
                    -- Query
                    remoteState <- providerQuery reference1
                    case remoteState of
                      DoesNotExistRemotely -> expectationFailure "should have existed remotely"
                      ExistsRemotely output -> output `shouldBe` output1

                    -- Check
                    checkResult1 <- providerCheck input1 output1
                    case checkResult1 of
                      CheckFailure err -> expectationFailure err
                      CheckSuccess -> pure ()

                    -- Change
                    applyResult <- providerApply input2 (ExistsLocallyAndRemotely reference1 output1)
                    case applyResult of
                      ApplyFailure err -> expectationFailure err
                      ApplySuccess reference2 output2 ->
                        let ctxAfterSecond = unlines ["After second apply;", "reference:", ppShow reference2, "output:", ppShow output2]
                         in context ctxAfterSecond $ do
                              -- Query again
                              remoteState <- providerQuery reference2
                              case remoteState of
                                DoesNotExistRemotely -> expectationFailure "should still have existed remotely"
                                ExistsRemotely output -> output `shouldBe` output2

                              -- Check
                              checkResult2 <- providerCheck input2 output2
                              case checkResult2 of
                                CheckFailure err -> expectationFailure err
                                CheckSuccess -> pure ()

                              -- Destroy
                              destroyResult <- providerDestroy reference2 (ExistsRemotely output2)
                              destroyResult `shouldBe` DestroySuccess
