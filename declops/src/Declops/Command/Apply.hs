{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Apply (declopsApply, declopsApplyResults) where

import Control.Monad
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

declopsApply :: C ()
declopsApply = do
  results <- declopsApplyResults

  let header = map (underline . fore blue) ["provider", "resource", "result"]
  putTable $
    header :
    map
      ( \(ResourceId {..}, result) ->
          [ providerNameChunk resourceIdProvider,
            resourceNameChunk resourceIdResource,
            applyResultChunk result
          ]
      )
      (M.toList results)

  if any applyFailed results
    then liftIO exitFailure
    else pure ()

applyResultChunk :: ApplyResult output -> Chunk
applyResultChunk = \case
  ApplyFailure _ -> fore red "failed"
  ApplySuccess _ -> fore green "success"

declopsApplyResults :: C (Map ResourceId JSONApplyResult)
declopsApplyResults = do
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
                  dependencyResult <- readMVar outputVar
                  pure (resourceIdProvider dependency, M.singleton (resourceIdResource dependency) dependencyResult)

          logDebugN "All dependencies satisfied to try to apply."

          let mDependencyOutputs :: Maybe (Map ProviderName (Map ResourceName JSON.Value))
              mDependencyOutputs =
                for dependencyResults $ \deps -> for deps $ \case
                  ApplySuccess output -> Just output
                  ApplyFailure _ -> Nothing

          result <- case mDependencyOutputs of
            Nothing -> do
              logWarnN "Not applying because some dependency failed to apply."
              pure $
                ApplyFailure $
                  ProviderException $
                    unwords
                      [ "Could not apply because a dependency failed to apply:",
                        T.unpack $ renderResourceId resourceId
                      ]
            Just dependencyOutputs -> do
              specification <- nixEvalResourceSpecification dependencyOutputs resourceId

              logInfoN $
                T.pack $
                  unlines
                    [ "Applying",
                      "specification:",
                      showJSON specification
                    ]

              logDebugN "Apply: Starting"
              result <- lift $ runProviderApply provider resourceName specification
              logDebugN "Apply: Done"
              pure result

          -- Make the apply result available for dependents to be applied as well.
          outputVar <- case M.lookup resourceId outputVars of
            Nothing -> liftIO $ die $ unwords ["Somehow no outputvar for resource", T.unpack $ renderResourceId resourceId]
            Just ov -> pure ov
          putMVar outputVar result

          case result of
            ApplyFailure err ->
              logErrorN $
                T.pack $
                  unlines
                    [ "Failed to apply:",
                      displayException err
                    ]
            ApplySuccess output ->
              logInfoN $
                T.pack $
                  unlines
                    [ "Applied successfully:",
                      showJSON output
                    ]

          pure (resourceId, result)
