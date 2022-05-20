{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Query (declopsQuery) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.OptParse
import Declops.Provider

declopsQuery :: QuerySettings -> C ()
declopsQuery QuerySettings {..} = do
  logDebugN "Parsing specification"
  specifications <- nixEval querySettingDeploymentFile

  trips <- forM specifications $
    \(SomeSpecification resourceTypeName resourceName _ provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Querying current state of",
              concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName resourceName

      remoteState <- case mLocalResource of
        Nothing -> pure DoesNotExistLocallyNorRemotely
        Just (Entity _ resource) -> do
          let reference = resourceReference resource
          remoteState <- liftIO $ providerQuery provider reference
          pure $ case remoteState of
            DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
            ExistsRemotely output -> ExistsLocallyAndRemotely reference output
      pure (resourceName, remoteState)
  liftIO $ mapM_ print trips
