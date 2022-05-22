{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Destroy (declopsDestroy, declopsDestroyResults) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Nix
import Declops.Provider
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
  dependencies <- nixEvalGraph
  dependenciesWithProviders <- case addProvidersToDependenciesSpecification dependencies of
    Left err -> liftIO $ die err
    Right d -> pure d

  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesWithProviders) $ \(_, (Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, _) -> do
        mLocalResource <- runDB $ getBy $ UniqueResourceReference providerName resourceName
        result <- case mLocalResource of
          Nothing -> do
            -- There was nothing to destroy, so we don't do anything but still
            -- consider it a success.
            -- If we were to fail here, the destroy command could not be
            -- idempotent.
            logDebugN $
              T.pack $
                unwords
                  [ "Not destroying because it doesn't exist locally:",
                    concat [T.unpack $ unProviderName providerName, ".", T.unpack $ unResourceName resourceName]
                  ]
            pure DestroySuccess
          Just (Entity resourceId resourceReference) -> do
            logInfoN $
              T.pack $
                unwords
                  [ "Destroying",
                    concat [T.unpack $ unProviderName providerName, ".", T.unpack $ unResourceName resourceName]
                  ]
            destroyResult <- liftIO $ providerDestroy (resourceReferenceReference resourceReference)
            runDB $ delete resourceId
            pure destroyResult
        pure (ResourceId providerName resourceName, result)
