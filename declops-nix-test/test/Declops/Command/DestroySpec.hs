module Declops.Command.DestroySpec (spec) where

import Control.Monad.Reader
import Declops.Command.Destroy
import Declops.Command.TestUtils
import Declops.Provider
import Test.Syd

spec :: Spec
spec = sequential $ do
  it "Works in this simple case" $
    testC "simple-success.nix" declopsDestroy

  pending "test for exit code"

  it "Succeeds from a clean slate" $
    testC "simple-success.nix" $ do
      results <- declopsDestroyResults
      liftIO $ results `shouldSatisfy` (not . any destroyFailed)

  it "Is idempotent when it succeeds" $
    testC "simple-success.nix" $ do
      results1 <- declopsDestroyResults
      results2 <- declopsDestroyResults
      liftIO $ results1 `shouldBe` results2
