module Declops.Command.CheckSpec (spec) where

import Control.Monad.Reader
import Declops.Command.Check
import Declops.Command.TestUtils
import Declops.Provider
import System.Exit
import Test.Syd

spec :: Spec
spec = sequential $ do
  it "Works in this simple case" $
    testC "simple-success.nix" declopsCheck `shouldThrow` (== ExitFailure 1)

  pending "test for exit code"

  it "Fails from a clean slate" $
    testC "simple-success.nix" $ do
      results <- declopsCheckResults
      liftIO $ results `shouldSatisfy` all checkFailed

  it "Is idempotent in a clean slate" $
    testC "simple-success.nix" $ do
      results1 <- declopsCheckResults
      results2 <- declopsCheckResults
      liftIO $ results1 `shouldBe` results2
