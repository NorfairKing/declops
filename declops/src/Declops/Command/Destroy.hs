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
      logDebugN $
        T.pack $
          unwords
            [ "Destroying current state of",
              concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName currentResourceName
      destroyResult <- case mLocalResource of
        Nothing ->
          -- There was nothing to destroy, so we don't do anything but still
          -- consider it a success.
          -- If we were to fail here, the destroy command could not be
          -- idempotent.
          pure DestroySuccess
        Just (Entity _ resource) ->
          liftIO $ providerDestroy provider (resourceReference resource)

      pure (currentResourceName, destroyResult)
  liftIO $ mapM_ print tups
