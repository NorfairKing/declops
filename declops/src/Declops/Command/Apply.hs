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
    \(SomeSpecification resourceTypeName resourceName specification provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Querying current state of",
              concat [T.unpack $ unProviderName resourceTypeName, ".", T.unpack $ unResourceName resourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResourceReference resourceTypeName resourceName

      applyContext <- case mLocalResource of
        Nothing -> pure DoesNotExistLocallyNorRemotely
        Just (Entity _ resourceReference) -> do
          let reference = resourceReferenceReference resourceReference
          remoteState <- liftIO $ providerQuery provider reference
          pure $ case remoteState of
            DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
            ExistsRemotely output -> ExistsLocallyAndRemotely reference output

      pure (SomeSpecification resourceTypeName resourceName specification provider, applyContext)

  results <- forConcurrently applyContexts $ \(SomeSpecification resourceTypeName resourceName specification provider, applyContext) -> do
    logInfoN $
      T.pack $
        unlines
          [ unwords
              [ "Applying",
                concat [T.unpack $ unProviderName resourceTypeName, ".", T.unpack $ unResourceName resourceName]
              ],
            showJSON specification
          ]
    applyResult <- liftIO $ providerApply provider specification applyContext
    case applyResult of
      ApplyFailure err ->
        logErrorN $
          T.pack $
            unlines
              [ unwords
                  [ "Failed to apply:",
                    concat [T.unpack $ unProviderName resourceTypeName, ".", T.unpack $ unResourceName resourceName]
                  ],
                err
              ]
      ApplySuccess reference output -> do
        logDebugN $
          T.pack $
            unlines
              [ unwords
                  [ "Applied successfully:",
                    concat [T.unpack $ unProviderName resourceTypeName, ".", T.unpack $ unResourceName resourceName]
                  ],
                showJSON reference,
                showJSON output
              ]
        _ <-
          runDB $
            upsertBy
              (UniqueResourceReference resourceTypeName resourceName)
              ( ResourceReference
                  { resourceReferenceName = resourceName,
                    resourceReferenceProvider = resourceTypeName,
                    resourceReferenceReference = toJSON reference
                  }
              )
              [ResourceReferenceReference =. toJSON reference]
        pure ()
    pure applyResult

  if any applyFailed results
    then liftIO exitFailure
    else pure ()
