{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Command.Query (declopsQuery, declopsQueryResults) where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Declops.Env
import Declops.Nix
import Declops.Provider
import Declops.Provider.ResourceId
import Text.Colour
import UnliftIO

declopsQuery :: C ()
declopsQuery = do
  results <- declopsQueryResults
  let header = map (underline . fore blue) ["provider", "resource", "status"]
  putTable $
    header :
    map
      ( \(ResourceId {..}, queryResult) ->
          [ providerNameChunk resourceIdProvider,
            resourceNameChunk resourceIdResource,
            queryResultChunk queryResult
          ]
      )
      (M.toList results)

queryResultChunk :: QueryResult output -> Chunk
queryResultChunk = \case
  QueryFailure _ -> fore red "error"
  QuerySuccess rs -> case rs of
    DoesNotExistRemotely -> fore yellow "does not exist"
    ExistsRemotely _ -> fore green "exists"

declopsQueryResults :: C (Map ResourceId JSONQueryResult)
declopsQueryResults = do
  DependenciesSpecification dependenciesMap <- nixEvalGraph
  fmap (M.fromList . concat) $
    forConcurrently (M.toList dependenciesMap) $ \(_, (provider@Provider {..}, resources)) ->
      forConcurrently (M.toList resources) $ \(resourceName, _) ->
        let resourceId = ResourceId providerName resourceName
         in withResourceIdSource resourceId $ do
              logInfoN "Querying the current state."
              logDebugN "Query: Starting"
              queryResult <- lift $ runProviderQuery provider resourceName
              logDebugN "Query: Done"
              case queryResult of
                QueryFailure err ->
                  logErrorN $
                    T.pack $
                      unlines
                        [ "Failed to query:",
                          displayException err
                        ]
                QuerySuccess remoteState ->
                  case remoteState of
                    DoesNotExistRemotely -> logInfoN "Resource does not exist remotely"
                    ExistsRemotely output ->
                      logInfoN $
                        T.pack $
                          unlines
                            [ "Resource exists remotely",
                              showJSON output
                            ]
              pure (resourceId, queryResult)
