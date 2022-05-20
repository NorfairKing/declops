{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Query (declopsQuery) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import UnliftIO

declopsQuery :: C ()
declopsQuery = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  trips <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName resourceName _ provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Querying current state of",
              concat [T.unpack $ unProviderName resourceTypeName, ".", T.unpack $ unResourceName resourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResourceReference resourceTypeName resourceName

      remoteState <- case mLocalResource of
        Nothing -> pure DoesNotExistLocallyNorRemotely
        Just (Entity _ resourceReference) -> do
          let reference = resourceReferenceReference resourceReference
          remoteState <- liftIO $ providerQuery provider reference
          pure $ case remoteState of
            DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
            ExistsRemotely output -> ExistsLocallyAndRemotely reference output
      pure (resourceName, remoteState)
  liftIO $ mapM_ print trips
