module Declops.Command.QuerySpec (spec) where

import Control.Monad.Reader
import Declops.Command.Query
import Declops.Command.TestUtils
import Declops.Provider
import Test.Syd

spec :: Spec
spec = sequential $ do
  it "Works in this simple case" $
    testC "simple-success.nix" declopsQuery

  it "Sees that no remote resources exist before the first application" $
    testC "simple-success.nix" $ do
      results <- declopsQueryResults
      liftIO $ results `shouldSatisfy` all (== Right DoesNotExistLocallyNorRemotely)

  it "Is idempotent before the first application" $
    testC "simple-success.nix" $ do
      results1 <- declopsQueryResults
      results2 <- declopsQueryResults
      liftIO $ results1 `shouldBe` results2
