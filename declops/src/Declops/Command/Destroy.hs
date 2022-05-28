{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Destroy (declopsDestroy, declopsDestroyResults) where

import Control.Monad.IO.Class
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
  DependenciesSpecification dependenciesMap <- nixEvalGraph

  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesMap) $ \(_, (provider@Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, _) ->
        let resourceId = ResourceId providerName resourceName
         in withResourceIdSource resourceId $ do
              mLocalResource <- runDB $ getBy $ UniqueResourceReference providerName resourceName
              result <- case mLocalResource of
                Nothing -> do
                  -- There was nothing to destroy, so we don't do anything but still
                  -- consider it a success.
                  -- If we were to fail here, the destroy command could not be
                  -- idempotent.
                  logInfoN "Not destroying because it doesn't exist locally."
                  pure DestroySuccess
                Just (Entity resourceId resourceReference) -> do
                  logInfoN "Destroying."
                  logDebugN "Destroy: Starting"
                  destroyResult <- lift $ runProviderDestroy provider (resourceReferenceReference resourceReference)
                  logDebugN "Deleting local reference."
                  runDB $ delete resourceId
                  logDebugN "Destroy: Done"
                  pure destroyResult
              case result of
                DestroyFailure err -> logErrorN $ T.pack $ unlines ["Failed to destroy:", err]
                DestroySuccess -> logInfoN "Destroyed successfully."
              pure (ResourceId providerName resourceName, result)
