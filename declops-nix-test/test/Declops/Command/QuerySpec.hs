module Declops.Command.QuerySpec (spec) where

import Control.Monad.Reader
import Declops.Command.Query
import Declops.Command.TestUtils
import Declops.Provider
import Test.Syd

spec :: Spec
spec = do
  it "Sees that no remote resources exist before the first application" $
    testC "simple-success.nix" $ do
      results <- declopsQueryResults
      liftIO $ results `shouldSatisfy` all (== DoesNotExistLocallyNorRemotely)
