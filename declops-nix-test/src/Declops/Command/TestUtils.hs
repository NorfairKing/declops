{-# LANGUAGE RecordWildCards #-}

module Declops.Command.TestUtils where

import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sqlite
import Declops.DB
import Declops.Env
import Path
import Path.IO
import Paths_declops_nix_test
import Text.Colour

testC :: FilePath -> C a -> IO a
testC deploymentFile func = do
  envDeploymentFile <- getDataFileName ("deployments/" <> deploymentFile) >>= resolveFile'
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
