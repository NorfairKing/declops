{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Query (declopsQuery, getApplyContexts) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Map (Map)
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

  trips <- getApplyContexts dependenciesWithProviders
  forM_ (M.toList trips) $ \(resourceId, (_, _, applyContext)) -> do
    liftIO $ print (resourceId, applyContext) -- TODO nice output

getApplyContexts :: Map ProviderName (JSONProvider, Map ResourceName [ResourceId]) -> C (Map ResourceId (JSONProvider, [ResourceId], JSONApplyContext))
getApplyContexts dependenciesWithProviders =
  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesWithProviders) $ \(_, (provider@Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, dependencies) -> do
        let resourceId = ResourceId providerName resourceName
        mLocalResource <- runDB $ getBy $ UniqueResourceReference providerName resourceName

        applyContext <- case mLocalResource of
          Nothing -> do
            logDebugN $
              T.pack $
                unwords
                  [ "Not querying current state because we have no local reference of",
                    T.unpack $ renderResourceId resourceId
                  ]
            pure DoesNotExistLocallyNorRemotely
          Just (Entity _ resourceReference) -> do
            logDebugN $
              T.pack $
                unwords
                  [ "Querying current state of",
                    T.unpack $ renderResourceId resourceId
                  ]

            let reference = resourceReferenceReference resourceReference
            remoteState <- liftIO $ providerQuery reference
            pure $ case remoteState of
              DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
              ExistsRemotely output -> ExistsLocallyAndRemotely reference output
        pure (resourceId, (provider, dependencies, applyContext))
