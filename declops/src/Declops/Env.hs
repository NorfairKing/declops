{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql
import Database.Persist.Sqlite
import Declops.DB
import Declops.OptParse
import Declops.Provider
import Declops.Provider.TempDir
import Declops.Provider.TempFile
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

nixEval :: Path Abs File -> C [SomeSpecification]
nixEval file = do
  maps <- nixEvalRaw file
  case mapsToSpecificationPairs maps of
    Left err -> liftIO $ die err
    Right specs -> pure specs

nixEvalRaw :: Path Abs File -> C (Map Text (Map Text JSON.Value))
nixEvalRaw file = do
  (exitCode, bs) <-
    liftIO $
      readProcessStdout $
        proc
          "nix"
          [ "eval",
            "--json",
            "--file",
            fromAbsFile file,
            "resources"
          ]
  case exitCode of
    ExitFailure _ -> liftIO $ die "nix failed."
    ExitSuccess -> case JSON.eitherDecode bs of
      Left err -> liftIO $ die err
      Right output -> pure output

-- TODO list of errors instead of only a single one
mapsToSpecificationPairs :: Map Text (Map Text JSON.Value) -> Either String [SomeSpecification]
mapsToSpecificationPairs m =
  fmap concat $
    forM (M.toList m) $ \(resourceTypeName, resources) ->
      forM (M.toList resources) $ \(resourceName, resource) -> do
        case M.lookup resourceTypeName allProviders of
          Nothing -> Left $ "Unknown provider: " <> T.unpack resourceTypeName
          Just provider ->
            pure $ SomeSpecification resourceTypeName resourceName resource provider

allProviders :: Map Text JSONProvider
allProviders =
  let p ::
        ( FromJSON specification,
          FromJSON reference,
          ToJSON reference,
          FromJSON output,
          ToJSON output
        ) =>
        Provider specification reference output ->
        (Text, JSONProvider)

      p provider = (providerName provider, toJSONProvider provider)
   in M.fromList [p tempDirProvider, p tempFileProvider]

data SomeSpecification
  = SomeSpecification
      !Text -- Resource type name
      !Text -- Resource name
      !JSON.Value
      !JSONProvider

showJSON :: JSON.Value -> String
showJSON = T.unpack . TE.decodeUtf8 . LB.toStrict . JSON.encodePretty
