module Declops.Command.ApplySpec (spec) where

import Control.Monad.Reader
import Declops.Command.Apply
import Declops.Command.TestUtils
import Declops.Provider
import Test.Syd

spec :: Spec
spec = sequential $ do
  it "Works in this simple case" $
    testC "simple-success.nix" declopsApply

  pending "test for exit code"

  it "Succeeds from a clean slate" $
    testC "simple-success.nix" $ do
      results <- declopsApplyResults
      liftIO $ results `shouldSatisfy` (not . any applyFailed)

  it "Is idempotent in a clean slate" $
    testC "simple-success.nix" $ do
      results1 <- declopsApplyResults
      results2 <- declopsApplyResults
      liftIO $ results1 `shouldBe` results2
