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
import Declops.DB
import Declops.Env
import Declops.OptParse
import Declops.Provider
import Declops.Provider.TempDir
import System.Exit

declopsApply :: ApplySettings -> C ()
declopsApply ApplySettings {..} = do
  logDebugN "Parsing specification"

  specifications <- nixEval applySettingDeploymentFile

  applyContexts <- forM (M.toList (specifications :: Map Text TempDirSpecification)) $ \(name, tempDirSpec) -> do
    logDebugN $
      T.pack $
        unwords
          [ "Querying current state of",
            T.unpack name
          ]
    mLocalResource <- runDB $ getBy $ UniqueResource name (providerName tempDirProvider)

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
          runDB $
            upsertBy
              (UniqueResource name (providerName tempDirProvider))
              ( Resource
                  { resourceName = name,
                    resourceProvider = providerName tempDirProvider,
                    resourceReference = toJSON reference
                  }
              )
              [ResourceReference =. toJSON reference]
        pure ()

-- data Specification = Specification
--   { specificationResources :: !(Map Text (Map Text JSON.Value))
--   }
--   deriving stock (Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (Autodocodec Specification)
