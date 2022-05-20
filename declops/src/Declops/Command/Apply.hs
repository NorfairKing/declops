{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Apply (declopsApply) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import System.Exit
import UnliftIO

declopsApply :: C ()
declopsApply = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  applyContexts <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName currentResourceName specification provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Querying current state of",
              concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName currentResourceName

      applyContext <- case mLocalResource of
        Nothing -> pure DoesNotExistLocallyNorRemotely
        Just (Entity _ resource) -> do
          let reference = resourceReference resource
          remoteState <- liftIO $ providerQuery provider reference
          pure $ case remoteState of
            DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
            ExistsRemotely output -> ExistsLocallyAndRemotely reference output

      pure (SomeSpecification resourceTypeName currentResourceName specification provider, applyContext)

  forConcurrently_ applyContexts $ \(SomeSpecification resourceTypeName currentResourceName specification provider, applyContext) -> do
    logInfoN $
      T.pack $
        unlines
          [ unwords
              [ "Applying",
                concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
              ],
            showJSON specification
          ]
    applyResult <- liftIO $ providerApply provider specification applyContext
    case applyResult of
      ApplyFailure err -> do
        logErrorN $
          T.pack $
            unwords
              [ "Failed to apply:",
                concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
              ]
        liftIO $ die err
      ApplySuccess reference output -> do
        logDebugN $
          T.pack $
            unlines
              [ unwords
                  [ "Applied successfully:",
                    concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
                  ],
                showJSON reference,
                showJSON output
              ]
        _ <-
          runDB $
            upsertBy
              (UniqueResource resourceTypeName currentResourceName)
              ( Resource
                  { resourceName = currentResourceName,
                    resourceProvider = resourceTypeName,
                    resourceReference = toJSON reference
                  }
              )
              [ResourceReference =. toJSON reference]
        pure ()
