{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Apply (declopsApply) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import System.Exit

declopsApply :: C ()
declopsApply = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  applyContexts <- forM specifications $
    \(SomeSpecification resourceTypeName resourceName specification provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Querying current state of",
              concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName resourceName

      applyContext <- case mLocalResource of
        Nothing -> pure DoesNotExistLocallyNorRemotely
        Just (Entity _ resource) -> do
          let reference = resourceReference resource
          remoteState <- liftIO $ providerQuery provider reference
          pure $ case remoteState of
            DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
            ExistsRemotely output -> ExistsLocallyAndRemotely reference output

      pure (SomeSpecification resourceTypeName resourceName specification provider, applyContext)

  forM_ applyContexts $ \(SomeSpecification resourceTypeName resourceName specification provider, applyContext) -> do
    logInfoN $
      T.pack $
        unlines
          [ unwords
              [ "Applying",
                concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
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
                concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
              ]
        liftIO $ die err
      ApplySuccess reference output -> do
        logDebugN $
          T.pack $
            unlines
              [ unwords
                  [ "Applied successfully:",
                    concat [T.unpack resourceTypeName, ".", T.unpack resourceName]
                  ],
                showJSON reference,
                showJSON output
              ]
        _ <-
          runDB $
            upsertBy
              (UniqueResource resourceTypeName resourceName)
              ( Resource
                  { resourceName = resourceName,
                    resourceProvider = resourceTypeName,
                    resourceReference = toJSON reference
                  }
              )
              [ResourceReference =. toJSON reference]
        pure ()
