{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Query (declopsQuery) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.OptParse
import Declops.Provider
import Declops.Provider.TempDir
import System.Exit

declopsQuery :: QuerySettings -> C ()
declopsQuery QuerySettings {..} = do
  logDebugN "Parsing specification"
  specifications <- nixEval querySettingDeploymentFile

  queryContexts <- forM (M.toList (specifications :: Map Text TempDirSpecification)) $ \(name, tempDirSpec) -> do
    logDebugN $
      T.pack $
        unwords
          [ "Querying current state of",
            T.unpack name
          ]
    mLocalResource <- runDB $ getBy $ UniqueResource name (providerName tempDirProvider)

    queryContext <- case mLocalResource of
      Nothing -> pure DoesNotExistLocallyNorRemotely
      Just (Entity _ Resource {..}) -> do
        reference <- case JSON.parseEither parseJSON resourceReference of
          Left err -> liftIO $ die err
          Right reference -> pure reference
        remoteState <- liftIO $ providerQuery tempDirProvider reference
        pure $ case remoteState of
          DoesNotExistRemotely -> ExistsLocallyButNotRemotely reference
          ExistsRemotely output -> ExistsLocallyAndRemotely reference output
    pure (name, tempDirSpec, queryContext)
  liftIO $ mapM_ print queryContexts
