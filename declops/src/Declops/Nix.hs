{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Nix where

import Autodocodec
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
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

parseDependenciesSpecification :: Map ProviderName (Map ResourceName [ResourceId]) -> Either DependenciesSpecificationError DependenciesSpecification
parseDependenciesSpecification = Right . DependenciesSpecification

data DependenciesSpecificationError = DependenciesSpecificationError
  deriving (Show, Eq, Generic)

instance Validity DependenciesSpecificationError

newtype DependenciesSpecification = DependenciesSpecification
  { unDependenciesSpecification :: Map ProviderName (Map ResourceName [ResourceId])
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec DependenciesSpecification)

instance Validity DependenciesSpecification

instance HasCodec DependenciesSpecification where
  codec = dimapCodec DependenciesSpecification unDependenciesSpecification codec

addProvidersToDependenciesSpecification :: DependenciesSpecification -> Either String (Map ProviderName (JSONProvider, Map ResourceName [ResourceId]))
addProvidersToDependenciesSpecification (DependenciesSpecification m) = fmap M.fromList $
  forM (M.toList m) $ \(providerName, resources) -> case M.lookup providerName allProviders of
    Nothing -> Left $ unwords ["Unknown provider:", T.unpack $ unProviderName providerName] -- TODO multiple errors
    Just provider -> Right (providerName, (provider, resources))

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
