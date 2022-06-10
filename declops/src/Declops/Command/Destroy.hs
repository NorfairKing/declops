{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Destroy (declopsDestroy, declopsDestroyResults) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
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

declopsDestroy :: C ()
declopsDestroy = do
  results <- declopsDestroyResults

  let header = map (underline . fore blue) ["provider", "resource", "result"]
  putTable $
    header :
    map
      ( \(ResourceId {..}, result) ->
          [ providerNameChunk resourceIdProvider,
            resourceNameChunk resourceIdResource,
            destroyResultChunk result
          ]
      )
      (M.toList results)

  if any destroyFailed results
    then liftIO exitFailure
    else pure ()

destroyResultChunk :: DestroyResult -> Chunk
destroyResultChunk = \case
  DestroyFailure _ -> fore red "failed"
  DestroySuccess -> fore green "success"

declopsDestroyResults :: C (Map ResourceId DestroyResult)
declopsDestroyResults = do
  DependenciesSpecification dependenciesMap <- reverseDependenciesSpecification <$> nixEvalGraph

  resultVars <- fmap (M.fromList . concat) $
    forM (M.toList dependenciesMap) $ \(providerName, (_, resources)) ->
      forM (M.toList resources) $ \(resourceName, _) -> do
        let resourceId = ResourceId providerName resourceName
        resultVar <- newEmptyMVar
        pure (resourceId, resultVar)

  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesMap) $ \(_, (provider@Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, reverseDependencies) -> do
        let resourceId = ResourceId providerName resourceName
         in withResourceIdSource resourceId $ do
              reverseDependencyResults <- fmap M.fromList $
                forConcurrently reverseDependencies $ \reverseDependency -> do
                  case M.lookup reverseDependency resultVars of
                    Nothing -> liftIO $ die $ unwords ["Unsatisfiable dependency", T.unpack $ renderResourceId reverseDependency]
                    Just resultVar -> do
                      logDebugN $
                        T.pack $
                          unwords
                            [ "Waiting for reverse dependency to be destroyed:",
                              T.unpack $ renderResourceId reverseDependency
                            ]
                      result <- readMVar resultVar
                      pure (reverseDependency, result)

              logDebugN "All reverse dependencies destroyed."

              let mReverseDependenciesOk :: Maybe (Map ResourceId ())
                  mReverseDependenciesOk = for reverseDependencyResults $ \case
                    DestroySuccess -> Just ()
                    DestroyFailure _ -> Nothing

              result <- case mReverseDependenciesOk of
                Nothing -> do
                  logWarnN "Not destroying because some reverse dependency failed to destroy"
                  pure $
                    DestroyFailure $
                      ProviderException $
                        unwords
                          [ "Could not destroy because a reverse dependency failed to destroy:",
                            T.unpack $ renderResourceId resourceId
                          ]
                Just _ -> do
                  logInfoN "Destroying."
                  logDebugN "Destroy: Starting"
                  destroyResult <- lift $ runProviderDestroy provider resourceName
                  logDebugN "Destroy: Done"
                  pure destroyResult

              resultVar <- case M.lookup resourceId resultVars of
                Nothing -> liftIO $ die $ unwords ["Somehow no resultvar for resource", T.unpack $ renderResourceId resourceId]
                Just rv -> pure rv
              putMVar resultVar result

              case result of
                DestroyFailure err -> logErrorN $ T.pack $ unlines ["Failed to destroy:", displayException err]
                DestroySuccess -> logInfoN "Destroyed successfully."

              pure (ResourceId providerName resourceName, result)
