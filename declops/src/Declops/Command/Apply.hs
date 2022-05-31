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
import Database.Persist
import Declops.Command.Query
import Declops.DB
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

applyResultChunk :: ApplyResult reference output -> Chunk
applyResultChunk = \case
  ApplyFailure _ -> fore red "failed"
  ApplySuccess _ _ -> fore green "success"

declopsApplyResults :: C (Map ResourceId JSONApplyResult)
declopsApplyResults = do
  DependenciesSpecification dependenciesMap <- nixEvalGraph

  errOrApplyContexts <- getApplyContexts dependenciesMap

  applyContexts <- forM errOrApplyContexts $ \(provider, dependencies, errOrApplyContext) -> case errOrApplyContext of
    -- TODO are we sure we want to die here?
    Left err -> liftIO $ die $ unwords ["Not applying because a query failed:", displayException err]
    Right applyContext -> pure (provider, dependencies, applyContext)

  outputVars <- forM applyContexts $ const newEmptyMVar

  fmap M.fromList $
    forConcurrently (M.toList applyContexts) $ \(resourceId, (provider, dependencies, applyContext)) ->
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
              for dependencyResults $ \resources -> for resources $ \case
                ApplySuccess _ output -> Just output
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
            result <- lift $ runProviderApply provider (resourceIdResource resourceId) specification applyContext
            logDebugN "Apply: Done"
            pure result

        -- Make the apply result available for dependents to be applied as well.
        outputVar <- case M.lookup resourceId outputVars of
          Nothing -> liftIO $ die $ unwords ["Somehow no outputvar for resource", T.unpack $ renderResourceId resourceId]
          Just ov -> pure ov
        putMVar outputVar result

        -- Put the resulting reference in our local database if applying succeeded.
        case result of
          ApplyFailure err ->
            logErrorN $
              T.pack $
                unlines
                  [ "Failed to apply:",
                    displayException err
                  ]
          ApplySuccess reference output -> do
            logInfoN $
              T.pack $
                unlines
                  [ "Applied successfully:",
                    showJSON reference,
                    showJSON output
                  ]
            logDebugN "Updating local reference."
            _ <-
              runDB $
                upsertBy
                  (UniqueResourceReference (resourceIdProvider resourceId) (resourceIdResource resourceId))
                  ( ResourceReference
                      { resourceReferenceProvider = resourceIdProvider resourceId,
                        resourceReferenceName = resourceIdResource resourceId,
                        resourceReferenceReference = toJSON reference
                      }
                  )
                  [ResourceReferenceReference =. toJSON reference]
            pure ()

        pure (resourceId, result)
