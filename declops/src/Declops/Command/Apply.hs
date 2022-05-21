{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Apply (declopsApply) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Persist
import Declops.Command.Query
import Declops.DB
import Declops.Env
import Declops.Provider
import System.Exit
import UnliftIO

declopsApply :: C ()
declopsApply = do
  logDebugN "Parsing specification"
  dependencyGraph <- nixEvalGraph
  dependenciesWithProviders <- case addProvidersToDependenciesSpecification dependencyGraph of
    Left err -> liftIO $ die err
    Right d -> pure d

  applyContexts <- getApplyContexts dependenciesWithProviders

  outputVars <- forM applyContexts $ const newEmptyMVar

  results <- forConcurrently (M.toList applyContexts) $ \(resourceId, (provider, dependencies, applyContext)) -> do
    dependencyOutputs <- fmap (M.fromListWith M.union) $
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
            dependencyOutput <- readMVar outputVar
            pure (resourceIdProvider dependency, M.singleton (resourceIdResource dependency) dependencyOutput)
    logDebugN $
      T.pack $
        unwords
          [ "All dependencies satisfied to try to apply",
            T.unpack $ renderResourceId resourceId
          ]

    specification <- nixEvalResourceSpecification dependencyOutputs resourceId

    logInfoN $
      T.pack $
        unlines
          [ unwords
              [ "Applying",
                T.unpack $ renderResourceId resourceId
              ],
            showJSON specification
          ]

    applyResult <- liftIO $ providerApply provider specification applyContext

    case applyResult of
      ApplyFailure err ->
        logErrorN $
          T.pack $
            unlines
              [ unwords
                  [ "Failed to apply:",
                    T.unpack $ renderResourceId resourceId
                  ],
                err
              ]
      ApplySuccess reference output -> do
        logDebugN $
          T.pack $
            unlines
              [ unwords
                  [ "Applied successfully:",
                    T.unpack $ renderResourceId resourceId
                  ],
                showJSON reference,
                showJSON output
              ]
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
        outputVar <- case M.lookup resourceId outputVars of
          Nothing -> liftIO $ die $ unwords ["Somehow no outputvar for resource", T.unpack $ renderResourceId resourceId]
          Just ov -> pure ov
        putMVar outputVar output
        pure ()

    pure applyResult

  if any applyFailed results
    then liftIO exitFailure
    else pure ()
