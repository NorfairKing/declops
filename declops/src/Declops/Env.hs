{-# LANGUAGE RecordWildCards #-}

module Declops.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Declops.DB
import Declops.OptParse
import Path
import System.Exit
import System.Process.Typed

type C a = ReaderT Env (LoggingT IO) a

data Env = Env
  { envConnectionPool :: !ConnectionPool
  }

runDB :: SqlPersistT IO a -> C a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool

runC :: Settings -> C () -> IO ()
runC Settings {..} func = do
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel) $
      withSqlitePool (T.pack (fromAbsFile settingStateFile)) 1 $ \pool -> do
        runSqlPool (runMigration localMigration) pool
        let envConnectionPool = pool
        runReaderT func Env {..}

nixEval :: FromJSON a => Path Abs File -> C a
nixEval file = do
  (exitCode, bs) <-
    liftIO $
      readProcessStdout $
        proc
          "nix"
          [ "eval",
            "--json",
            "--file",
            fromAbsFile file,
            "resources.temporary-directory"
          ]
  case exitCode of
    ExitFailure _ -> liftIO $ die "nix failed."
    ExitSuccess -> case JSON.eitherDecode bs of
      Left err -> liftIO $ die err
      Right output -> pure output
