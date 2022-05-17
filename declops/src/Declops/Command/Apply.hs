{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Apply (declopsApply) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Declops.DB
import Declops.OptParse
import Declops.Provider
import Declops.Provider.TempDir
import Path
import System.Exit

declopsApply :: Settings -> IO ()
declopsApply Settings {..} = do
  runStderrLoggingT $
    withSqlitePool (T.pack (fromAbsFile settingStateFile)) 1 $ \pool -> do
      runSqlPool (runMigration localMigration) pool
      let (name, tempDirSpec) = sampleConfig
      mLocalResource <- runSqlPool (getBy $ UniqueResource name (providerName tempDirProvider)) pool
      liftIO $ do
        applyContext <- case mLocalResource of
          Nothing -> pure DoesNotExistLocallyNorRemotely
          Just (Entity _ Resource {..}) -> do
            reference <- case JSON.parseEither parseJSON resourceReference of
              Left err -> die err
              Right reference -> pure reference
            remoteState <- providerQuery tempDirProvider reference
            pure $ case remoteState of
              DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
              ExistsRemotely output -> ExistsLocallyAndRemotely reference output

        applyResult <- providerApply tempDirProvider tempDirSpec applyContext
        case applyResult of
          ApplyFailure err -> die err
          ApplySuccess reference output -> do
            _ <-
              runSqlPool
                ( upsertBy
                    (UniqueResource name (providerName tempDirProvider))
                    ( Resource
                        { resourceName = name,
                          resourceProvider = providerName tempDirProvider,
                          resourceReference = toJSON reference
                        }
                    )
                    [ResourceReference =. toJSON reference]
                )
                pool
            print (reference, output)

sampleConfig :: (Text, TempDirSpecification)
sampleConfig =
  ( "my-dir",
    TempDirSpecification
      { tempDirSpecificationBase = [absdir|/tmp|],
        tempDirSpecificationTemplate = "foobar"
      }
  )
