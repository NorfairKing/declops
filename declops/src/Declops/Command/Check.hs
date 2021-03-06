{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Check (declopsCheck, declopsCheckResults) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable
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

  outputVars <-
    fmap (M.fromList . concat) $
      forM (M.toList dependenciesMap) $ \(providerName, (_, resources)) ->
        forM (M.toList resources) $ \(resourceName, _) -> do
          outputVar <- newEmptyMVar
          pure (ResourceId providerName resourceName, outputVar)

  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesMap) $ \(providerName, (provider, resources)) -> do
      forConcurrently (M.toList resources) $ \(resourceName, dependencies) -> do
        let resourceId = ResourceId providerName resourceName
        withResourceIdSource resourceId $ do
          dependencyResults <- fmap (M.fromListWith M.union) $
            forConcurrently dependencies $ \dependency -> do
              case M.lookup dependency outputVars of
                Nothing -> liftIO $ die $ unwords ["Unsatisfiable dependency", T.unpack $ renderResourceId dependency]
                Just outputVar -> do
                  logDebugN $
                    T.pack $
                      unwords
                        [ "Waiting for the outputs of",
                          T.unpack $ renderResourceId dependency
                        ]
                  dependencyResults <- readMVar outputVar
                  pure (resourceIdProvider dependency, M.singleton (resourceIdResource dependency) dependencyResults)

          logDebugN "All dependencies satisfied to try to check"

          let mDependencyOutputs :: Maybe (Map ProviderName (Map ResourceName JSON.Value))
              mDependencyOutputs =
                for dependencyResults $ \deps -> for deps $ \case
                  CheckSuccess output -> Just output
                  CheckFailure _ -> Nothing

          result <- case mDependencyOutputs of
            Nothing -> do
              logWarnN "Not checking because some dependency failed to check."
              pure $
                CheckFailure $
                  ProviderException $
                    unwords
                      [ "Could not check because a dependency failed to check:",
                        T.unpack $ renderResourceId resourceId
                      ]
            Just dependencyOutputs -> do
              specification <- nixEvalResourceSpecification dependencyOutputs resourceId
              logInfoN "Checking"
              logDebugN "Check: Starting"
              result <- lift $ runProviderCheck provider (resourceIdResource resourceId) specification
              logDebugN "Check: Done"
              pure result

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
                    [ "Check failed:",
                      displayException err
                    ]
            CheckSuccess _ -> logInfoN "Check succeeded."

          pure (resourceId, result)
