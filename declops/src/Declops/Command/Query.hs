{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Query (declopsQuery) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import System.Exit
import UnliftIO

declopsQuery :: C ()
declopsQuery = do
  dependencies <- nixEvalGraph
  dependenciesWithProviders <- case addProvidersToDependenciesSpecification dependencies of
    Left err -> liftIO $ die err
    Right d -> pure d

  trips <- fmap concat $
    forConcurrently (M.toList dependenciesWithProviders) $ \(_, (Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, _) -> do
        logDebugN $
          T.pack $
            unwords
              [ "Querying current state of",
                concat [T.unpack $ unProviderName providerName, ".", T.unpack $ unResourceName resourceName]
              ]
        mLocalResource <- runDB $ getBy $ UniqueResourceReference providerName resourceName

        remoteState <- case mLocalResource of
          Nothing -> pure DoesNotExistLocallyNorRemotely
          Just (Entity _ resourceReference) -> do
            let reference = resourceReferenceReference resourceReference
            remoteState <- liftIO $ providerQuery reference
            pure $ case remoteState of
              DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
              ExistsRemotely output -> ExistsLocallyAndRemotely reference output
        pure (resourceName, remoteState)
  liftIO $ mapM_ print trips
