{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Query (declopsQuery, declopsQueryResults, getApplyContexts) where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Nix
import Declops.Provider
import Declops.Provider.ResourceId
import Text.Colour
import UnliftIO

declopsQuery :: C ()
declopsQuery = do
  results <- declopsQueryResults
  let header = map (underline . fore blue) ["provider", "resource", "status"]
  putTable $
    header :
    map
      ( \(ResourceId {..}, errOrApplyContext) ->
          [ providerNameChunk resourceIdProvider,
            resourceNameChunk resourceIdResource,
            errOrApplyContextChunk errOrApplyContext
          ]
      )
      (M.toList results)

errOrApplyContextChunk :: Either ProviderException (ApplyContext reference output) -> Chunk
errOrApplyContextChunk = \case
  Left _ -> fore red "error"
  Right ac -> applyContextChunk ac

applyContextChunk :: ApplyContext reference output -> Chunk
applyContextChunk = \case
  DoesNotExistLocallyNorRemotely -> fore yellow "does not exist"
  ExistsLocallyButNotRemotely _ -> fore red "missing"
  ExistsLocallyAndRemotely _ _ -> fore green "exists"

declopsQueryResults :: C (Map ResourceId (Either ProviderException JSONApplyContext))
declopsQueryResults = do
  DependenciesSpecification dependenciesMap <- nixEvalGraph
  M.map (\(_, _, ac) -> ac) <$> getApplyContexts dependenciesMap

getApplyContexts ::
  Map ProviderName (JSONProvider, Map ResourceName [ResourceId]) ->
  C (Map ResourceId (JSONProvider, [ResourceId], Either ProviderException JSONApplyContext))
getApplyContexts dependenciesWithProviders =
  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesWithProviders) $ \(_, (provider@Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, dependencies) ->
        let resourceId = ResourceId providerName resourceName
         in withResourceIdSource resourceId $ do
              mLocalResource <- runDB $ getBy $ UniqueResourceReference providerName resourceName

              errOrApplyContext <- case mLocalResource of
                Nothing -> do
                  logInfoN "Not querying the current state because we have no local reference."
                  pure $ Right DoesNotExistLocallyNorRemotely
                Just (Entity _ resourceReference) -> do
                  logInfoN "Querying the current state."
                  let reference = resourceReferenceReference resourceReference
                  logDebugN "Query: Starting"
                  queryResult <- lift $ runProviderQuery provider resourceName reference
                  logDebugN "Query: Done"
                  case queryResult of
                    QueryFailure err -> do
                      logErrorN $
                        T.pack $
                          unlines
                            [ "Failed to query:",
                              displayException err
                            ]
                      pure $ Left err
                    QuerySuccess remoteState ->
                      Right <$> case remoteState of
                        DoesNotExistRemotely -> do
                          logInfoN $
                            T.pack $
                              unlines
                                [ "Resource exists locally but not remotely",
                                  "reference:",
                                  showJSON reference
                                ]
                          pure $ ExistsLocallyButNotRemotely reference
                        ExistsRemotely output -> do
                          logInfoN $
                            T.pack $
                              unlines
                                [ "Resource exists locally and remotely",
                                  "reference:",
                                  showJSON reference,
                                  "output:",
                                  showJSON output
                                ]
                          pure $ ExistsLocallyAndRemotely reference output
              pure (resourceId, (provider, dependencies, errOrApplyContext))
