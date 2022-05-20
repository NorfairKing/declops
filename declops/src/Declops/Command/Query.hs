{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Query (declopsQuery) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import UnliftIO

declopsQuery :: C ()
declopsQuery = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  trips <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName currentResourceName _ provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Querying current state of",
              concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName currentResourceName

      remoteState <- case mLocalResource of
        Nothing -> pure DoesNotExistLocallyNorRemotely
        Just (Entity _ resource) -> do
          let reference = resourceReference resource
          remoteState <- liftIO $ providerQuery provider reference
          pure $ case remoteState of
            DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
            ExistsRemotely output -> ExistsLocallyAndRemotely reference output
      pure (currentResourceName, remoteState)
  liftIO $ mapM_ print trips
