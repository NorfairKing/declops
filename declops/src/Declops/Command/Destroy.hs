{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Destroy (declopsDestroy) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import UnliftIO

declopsDestroy :: C ()
declopsDestroy = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  tups <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName currentResourceName _ provider) -> do
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName currentResourceName
      destroyResult <- case mLocalResource of
        Nothing -> do
          -- There was nothing to destroy, so we don't do anything but still
          -- consider it a success.
          -- If we were to fail here, the destroy command could not be
          -- idempotent.
          logDebugN $
            T.pack $
              unwords
                [ "Not destroying because it doesn't exist locally:",
                  concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
                ]
          pure DestroySuccess
        Just (Entity resourceId resource) -> do
          logInfoN $
            T.pack $
              unwords
                [ "Destroying",
                  concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
                ]
          destroyResult <- liftIO $ providerDestroy provider (resourceReference resource)
          runDB $ delete resourceId
          pure destroyResult

      pure (currentResourceName, destroyResult)
  liftIO $ mapM_ print tups
