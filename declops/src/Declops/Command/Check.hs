{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Check (declopsCheck) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import UnliftIO

declopsCheck :: C ()
declopsCheck = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  tups <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName currentResourceName specification provider) -> do
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName currentResourceName

      checkResult <- case mLocalResource of
        Nothing ->
          pure $
            CheckFailure $
              unwords
                [ "Resource does not exist locally: "
                    <> concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
                ]
        Just (Entity _ resource) -> do
          logDebugN $
            T.pack $
              unwords
                [ "Checking",
                  concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
                ]
          let reference = resourceReference resource
          liftIO $ providerCheck provider specification reference
      pure (currentResourceName, checkResult)
  liftIO $ mapM_ print tups
