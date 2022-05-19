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

  applyContexts <- forM specifications $ \(SomeSpecification resourceTypeName resourceName specification provider) -> do
    logDebugN $
      T.pack $
        unwords
          [ "Querying current state of",
            concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
          ]
    mLocalResource <- runDB $ getBy $ UniqueResource resourceName resourceTypeName

    applyContext <- case mLocalResource of
      Nothing -> pure DoesNotExistLocallyNorRemotely
      Just (Entity _ Resource {..}) -> do
        reference <- case JSON.parseEither parseJSON resourceReference of
          Left err -> liftIO $ die err
          Right reference -> pure reference
        remoteState <- liftIO $ providerQuery provider reference
        pure $ case remoteState of
          DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
          ExistsRemotely output -> ExistsLocallyAndRemotely reference output

    pure $ SomeSpecificationWithApplyContext resourceTypeName resourceName specification provider applyContext

  forM_ applyContexts $ \(SomeSpecificationWithApplyContext resourceTypeName resourceName specification provider applyContext) -> do
    logInfoN $
      T.pack $
        unwords
          [ "Applying",
            concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
          ]
    applyResult <- liftIO $ providerApply provider specification applyContext
    case applyResult of
      ApplyFailure err -> do
        logErrorN $
          T.pack $
            unwords
              [ "Failed to apply:",
                concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
              ]
        liftIO $ die err
      ApplySuccess reference output -> do
        logDebugN $
          T.pack $
            unlines
              [ unwords
                  [ "Applied successfully:",
                    concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
                  ],
                show output
              ]
        _ <-
          runDB $
            upsertBy
              (UniqueResource resourceName resourceTypeName)
              ( Resource
                  { resourceName = resourceName,
                    resourceProvider = resourceTypeName,
                    resourceReference = toJSON reference
                  }
              )
              [ResourceReference =. toJSON reference]
        pure ()
