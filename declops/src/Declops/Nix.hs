{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Nix where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Validity
import Declops.DB
import Declops.Env
import Declops.Provider
import Declops.Provider.ResourceId
import GHC.Generics (Generic)
import Path
import Path.IO
import Paths_declops
import System.Exit
import System.IO (hClose)
import System.Process.Typed

nixEvalGraph :: C DependenciesSpecification
nixEvalGraph = do
  deploymentFile <- asks envDeploymentFile
  getGraphFile <- liftIO $ getDataFileName "nix-bits/get-graph.nix"
  m <-
    nixEvalJSON
      [ "--file",
        getGraphFile,
        "dependencies",
        "--arg",
        "deploymentFile",
        fromAbsFile deploymentFile
      ]
  case parseDependenciesSpecification m of
    Left err -> liftIO $ die $ show err
    Right s -> pure s

parseDependenciesSpecification ::
  Map ProviderName (Map ResourceName [ResourceId]) ->
  Either DependenciesSpecificationError DependenciesSpecification
parseDependenciesSpecification dependenciesMap =
  case NE.nonEmpty (getMissingResources dependenciesMap) of
    Nothing -> case addProvidersToDependenciesSpecification dependenciesMap of
      Left unknownProviders -> Left $ DependenciesSpecificationUnknownProvider unknownProviders
      Right dependenciesSpecificationMap -> Right $ DependenciesSpecification dependenciesSpecificationMap
    Just missingResources -> Left $ DependenciesSpecificationMissingResources missingResources

getMissingResources ::
  Map ProviderName (Map ResourceName [ResourceId]) ->
  [ResourceId]
getMissingResources dependenciesMap =
  concatMap
    ( \(_, resources) ->
        concatMap
          ( \(_, dependencies) ->
              let isMissing ResourceId {..} = case M.lookup resourceIdProvider dependenciesMap of
                    Nothing -> True
                    Just resourcesMap -> not $ M.member resourceIdResource resourcesMap
               in filter isMissing dependencies
          )
          (M.toList resources)
    )
    (M.toList dependenciesMap)

addProvidersToDependenciesSpecification ::
  Map ProviderName a ->
  Either (NonEmpty ProviderName) (Map ProviderName (JSONProvider, a))
addProvidersToDependenciesSpecification dependenciesMap =
  let (unknownProviders, withProviders) = partitionEithers $
        flip map (M.toList dependenciesMap) $ \(providerName, resources) -> case M.lookup providerName allProviders of
          Nothing -> Left providerName
          Just provider -> Right (providerName, (provider, resources))
   in case NE.nonEmpty unknownProviders of
        Nothing -> Right $ M.fromList withProviders
        Just ne -> Left ne

data DependenciesSpecificationError
  = DependenciesSpecificationMissingResources !(NonEmpty ResourceId)
  | DependenciesSpecificationUnknownProvider !(NonEmpty ProviderName)
  deriving (Show, Eq, Generic)

instance Validity DependenciesSpecificationError

newtype DependenciesSpecification = DependenciesSpecification
  { unDependenciesSpecification :: Map ProviderName (JSONProvider, Map ResourceName [ResourceId])
  }
  deriving stock (Generic)

instance Validity DependenciesSpecification where
  validate ds@(DependenciesSpecification dependenciesMap) =
    mconcat
      [ genericValidate ds,
        declare "there are no missing resources in the graph" $
          null $
            concatMap
              ( \(_, (_, resources)) ->
                  concatMap
                    ( \(_, dependencies) ->
                        let isMissing ResourceId {..} = case M.lookup resourceIdProvider dependenciesMap of
                              Nothing -> True
                              Just (_, resourcesMap) -> not $ M.member resourceIdResource resourcesMap
                         in filter isMissing dependencies
                    )
                    (M.toList resources)
              )
              (M.toList dependenciesMap)
      ]

-- For printing
removeProviders :: DependenciesSpecification -> Map ProviderName (Map ResourceName [ResourceId])
removeProviders (DependenciesSpecification dependenciesMap) = M.map snd dependenciesMap

nixEvalResourceSpecification :: Map ProviderName (Map ResourceName JSON.Value) -> ResourceId -> C JSON.Value
nixEvalResourceSpecification outputs ResourceId {..} = do
  deploymentFile <- asks envDeploymentFile
  -- withSystemTempFile "declops-resource-specification-eval" $ \outputsFile outputsFileHandle -> do
  tmpDir <- resolveDir' "/tmp"
  (outputsFile, outputsFileHandle) <- openTempFile tmpDir "declops-resource-specification-eval"
  do
    liftIO $ do
      LB.hPut outputsFileHandle $ JSON.encodePretty outputs
      hClose outputsFileHandle
    getSpecificationFile <- liftIO $ getDataFileName "nix-bits/get-specification.nix"
    nixEvalJSON
      [ "--file",
        getSpecificationFile,
        "output",
        "--arg",
        "deploymentFile",
        fromAbsFile deploymentFile,
        "--arg",
        "outputsFile",
        fromAbsFile outputsFile,
        "--argstr",
        "providerName",
        T.unpack (unProviderName resourceIdProvider),
        "--argstr",
        "resourceName",
        T.unpack (unResourceName resourceIdResource)
      ]

nixEvalJSON :: FromJSON a => [String] -> C a
nixEvalJSON args = do
  let allArgs = "eval" : "--json" : args

  logDebugN $
    T.pack $
      unwords
        [ "Running",
          show $ unwords $ "nix" : allArgs
        ]
  (exitCode, bs) <-
    liftIO $
      readProcessStdout $
        proc "nix" allArgs

  case exitCode of
    ExitFailure _ -> liftIO $ die "nix failed."
    ExitSuccess -> case JSON.eitherDecode bs of
      Left err -> liftIO $ die err
      Right output -> pure output
