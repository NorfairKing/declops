{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Apply (declopsApply) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Declops.DB
import Declops.OptParse
import Declops.Provider
import Declops.Provider.TempDir
import Path
import System.Exit
import System.Process.Typed

declopsApply :: ApplySettings -> Settings -> IO ()
declopsApply ApplySettings {..} Settings {..} = do
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel) $
      withSqlitePool (T.pack (fromAbsFile settingStateFile)) 1 $ \pool -> do
        runSqlPool (runMigration localMigration) pool
        logDebugN "Parsing specification"
        (exitCode, bs) <-
          liftIO $
            readProcessStdout $
              proc
                "nix"
                [ "eval",
                  "--json",
                  "--file",
                  fromAbsFile applySettingDeploymentFile,
                  "resources.temporary-directory"
                ]
        case exitCode of
          ExitFailure _ ->
            liftIO $ die "nix failed." :: LoggingT IO ()
          ExitSuccess -> do
            specification <- case JSON.eitherDecode bs of
              Left err -> liftIO $ die err
              Right specification -> pure (specification :: Map Text TempDirSpecification)
            applyContexts <- forM (M.toList specification) $ \(name, tempDirSpec) -> do
              logDebugN $
                T.pack $
                  unwords
                    [ "Querying current state of",
                      T.unpack name
                    ]
              mLocalResource <- runSqlPool (getBy $ UniqueResource name (providerName tempDirProvider)) pool

              applyContext <- case mLocalResource of
                Nothing -> pure DoesNotExistLocallyNorRemotely
                Just (Entity _ Resource {..}) -> do
                  reference <- case JSON.parseEither parseJSON resourceReference of
                    Left err -> liftIO $ die err
                    Right reference -> pure reference
                  remoteState <- liftIO $ providerQuery tempDirProvider reference
                  pure $ case remoteState of
                    DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
                    ExistsRemotely output -> ExistsLocallyAndRemotely reference output

              pure (name, tempDirSpec, applyContext)

            forM_ applyContexts $ \(name, tempDirSpec, applyContext) -> do
              logInfoN $
                T.pack $
                  unwords
                    [ "Applying",
                      T.unpack name
                    ]
              applyResult <- liftIO $ providerApply tempDirProvider tempDirSpec applyContext
              case applyResult of
                ApplyFailure err -> do
                  logErrorN $
                    T.pack $
                      unwords
                        [ "Failed to apply:",
                          T.unpack name
                        ]
                  liftIO $ die err
                ApplySuccess reference output -> do
                  logDebugN $
                    T.pack $
                      unlines
                        [ unwords ["Applied successfully:", T.unpack name],
                          show output
                        ]
                  _ <-
                    runSqlPool
                      ( upsertBy
                          (UniqueResource name (providerName tempDirProvider))
                          ( Resource
                              { resourceName = name,
                                resourceProvider = providerName tempDirProvider,
                                resourceReference = toJSON reference
                              }
                          )
                          [ResourceReference =. toJSON reference]
                      )
                      pool
                  pure ()

-- data Specification = Specification
--   { specificationResources :: !(Map Text (Map Text JSON.Value))
--   }
--   deriving stock (Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (Autodocodec Specification)
