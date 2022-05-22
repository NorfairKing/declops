{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Check (declopsCheck, declopsCheckResults) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Nix
import Declops.Provider
import Declops.Provider.ResourceId
import System.Exit
import Text.Colour
import UnliftIO

declopsCheck :: C ()
declopsCheck = do
  results <- declopsCheckResults

  let header = map (underline . fore blue) ["provider", "resource", "result"]
  putTable $
    header :
    map
      ( \(ResourceId {..}, result) ->
          [ providerNameChunk resourceIdProvider,
            resourceNameChunk resourceIdResource,
            checkResultChunk result
          ]
      )
      (M.toList results)

  if any checkFailed results
    then liftIO exitFailure
    else pure ()

checkResultChunk :: CheckResult output -> Chunk
checkResultChunk = \case
  CheckFailure _ -> fore red "failed"
  CheckSuccess _ -> fore green "success"

declopsCheckResults :: C (Map ResourceId JSONCheckResult)
declopsCheckResults = do
  DependenciesSpecification dependenciesMap <- nixEvalGraph

  referenceMap <- getReferenceMap dependenciesMap

  outputVars <- forM referenceMap $ const newEmptyMVar

  fmap M.fromList $
    forConcurrently (M.toList referenceMap) $ \(resourceId, (provider, dependencies, mReference)) -> do
      dependencyResults <- fmap (M.fromListWith M.union) $
        forConcurrently dependencies $ \dependency -> do
          case M.lookup dependency outputVars of
            Nothing -> liftIO $ die $ unwords ["Unsatisfiable dependency", T.unpack $ renderResourceId dependency]
            Just outputVar -> do
              logDebugN $
                T.pack $
                  unwords
                    [ "Waiting for the outputs of",
                      T.unpack $ renderResourceId dependency,
                      "to apply",
                      T.unpack $ renderResourceId resourceId
                    ]
              dependencyResults <- readMVar outputVar
              pure (resourceIdProvider dependency, M.singleton (resourceIdResource dependency) dependencyResults)

      logDebugN $
        T.pack $
          unwords
            [ "All dependencies satisfied to try to check",
              T.unpack $ renderResourceId resourceId
            ]

      let mDependencyOutputs :: Maybe (Map ProviderName (Map ResourceName JSON.Value))
          mDependencyOutputs =
            for dependencyResults $ \resources -> for resources $ \case
              CheckSuccess output -> Just output
              CheckFailure _ -> Nothing

      result <- case mDependencyOutputs of
        Nothing -> do
          logDebugN $
            T.pack $
              unwords
                [ "Not checking because some dependency failed to check:",
                  T.unpack $ renderResourceId resourceId
                ]
          pure $
            CheckFailure $
              unwords
                [ "Could not check because a dependency failed to check:",
                  T.unpack $ renderResourceId resourceId
                ]
        Just dependencyOutputs -> do
          specification <- nixEvalResourceSpecification dependencyOutputs resourceId
          case mReference of
            DoesNotExistLocally -> do
              logDebugN $
                T.pack $
                  unlines
                    [ unwords
                        [ "Not checking because we had no local reference:",
                          T.unpack $ renderResourceId resourceId
                        ],
                      showJSON specification
                    ]
              pure $
                CheckFailure $
                  unwords
                    [ "Could not check because we had no local reference:",
                      T.unpack $ renderResourceId resourceId
                    ]
            ExistsLocally reference -> do
              logInfoN $
                T.pack $
                  unwords
                    [ "Checking",
                      T.unpack $ renderResourceId resourceId
                    ]
              liftIO $ providerCheck provider specification reference

      -- Make the check result available for dependents to be checked as well
      outputVar <- case M.lookup resourceId outputVars of
        Nothing -> liftIO $ die $ unwords ["Somehow no outputvar for resource", T.unpack $ renderResourceId resourceId]
        Just ov -> pure ov
      putMVar outputVar result

      -- Log the result
      case result of
        CheckFailure err ->
          logErrorN $
            T.pack $
              unlines
                [ unwords
                    [ "Check failed for:",
                      T.unpack $ renderResourceId resourceId
                    ],
                  err
                ]
        CheckSuccess _ ->
          logInfoN $
            T.pack $
              unwords
                [ "Check succeeded for:",
                  T.unpack $ renderResourceId resourceId
                ]

      pure (resourceId, result)

getReferenceMap :: Map ProviderName (JSONProvider, Map ResourceName [ResourceId]) -> C (Map ResourceId (JSONProvider, [ResourceId], LocalState JSON.Value))
getReferenceMap dependenciesWithProviders =
  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesWithProviders) $ \(_, (provider@Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, dependencies) -> do
        mLocalResource <- runDB $ getBy $ UniqueResourceReference providerName resourceName

        let localState = case mLocalResource of
              Nothing -> DoesNotExistLocally
              Just (Entity _ resourceReference) -> ExistsLocally $ resourceReferenceReference resourceReference

        pure (ResourceId providerName resourceName, (provider, dependencies, localState))
