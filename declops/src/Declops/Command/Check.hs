{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Check (declopsCheck) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import System.Exit
import UnliftIO

declopsCheck :: C ()
declopsCheck = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  results <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName resourceName specification provider) -> do
      mLocalResource <- runDB $ getBy $ UniqueResourceReference resourceTypeName resourceName

      case mLocalResource of
        Nothing ->
          pure $
            CheckFailure $
              unwords
                [ "Resource does not exist locally: "
                    <> concat [T.unpack resourceTypeName, ".", T.unpack $ unResourceName resourceName]
                ]
        Just (Entity _ resourceReference) -> do
          logDebugN $
            T.pack $
              unwords
                [ "Checking",
                  concat [T.unpack resourceTypeName, ".", T.unpack $ unResourceName resourceName]
                ]
          let reference = resourceReferenceReference resourceReference
          liftIO $ providerCheck provider specification reference

  if any checkFailed results
    then liftIO exitFailure
    else pure ()
