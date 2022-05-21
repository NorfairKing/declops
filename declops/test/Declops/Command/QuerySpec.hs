{-# LANGUAGE RecordWildCards #-}

module Declops.Command.QuerySpec (spec) where

import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sqlite
import Declops.Command.Query
import Declops.DB
import Declops.Env
import Declops.Provider
import Path
import Path.IO
import Test.Syd
import Text.Colour

spec :: Spec
spec = do
  it "Sees that no remote resources exist before the first application" $
    testC "simple-success.nix" $ do
      results <- declopsQueryResults
      liftIO $ results `shouldSatisfy` all (== DoesNotExistLocallyNorRemotely)

testC :: FilePath -> C a -> IO a
testC deploymentFile func = do
  testResourcesDeploymentDir <- resolveDir' "test_resources/deployments"
  envDeploymentFile <- resolveFile testResourcesDeploymentDir deploymentFile
  let logFunc loc source level str = do
        _ <- evaluate loc
        _ <- evaluate source
        _ <- evaluate level
        _ <- evaluate str
        pure () -- No logging, but still evaluating the arguments
  flip runLoggingT logFunc $
    withSystemTempDir "declops-command-test" $ \tdir -> do
      dbFile <- resolveFile tdir "db.sqlite3"
      withSqlitePool (T.pack (fromAbsFile dbFile)) 1 $ \pool -> do
        _ <- runSqlPool (runMigrationSilent localMigration) pool
        let envConnectionPool = pool
        let envTerminalCapabilities = WithoutColours
        runReaderT func Env {..}
