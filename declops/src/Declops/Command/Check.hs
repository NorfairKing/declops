{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Check (declopsCheck) where

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
import Declops.Provider
import System.Exit
import UnliftIO

declopsCheck :: C ()
declopsCheck = do
  logDebugN "Parsing specification"
  dependencyGraph <- nixEvalGraph
  dependenciesWithProviders <- case addProvidersToDependenciesSpecification dependencyGraph of
    Left err -> liftIO $ die err
    Right d -> pure d

  referenceMap <- getReferenceMap dependenciesWithProviders

  outputVars <- forM referenceMap $ const newEmptyMVar

  results <- forConcurrently (M.toList referenceMap) $ \(resourceId, (provider, dependencies, mReference)) -> do
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

    -- Log the resutl
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

    pure result

  if any checkFailed results
    then liftIO $ die $ unlines $ map show results
    else pure ()

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
