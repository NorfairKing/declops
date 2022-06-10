{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempDir
  ( TempDirSpecification (..),
    TempDirOutput (..),
    tempDirProvider,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO

data TempDirSpecification = TempDirSpecification
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempDirSpecification)

instance Validity TempDirSpecification

instance HasCodec TempDirSpecification where
  codec =
    object "TempDirSpecification" $
      pure TempDirSpecification

data TempDirOutput = TempDirOutput
  { tempDirOutputPath :: !(Path Abs Dir)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempDirOutput)

instance Validity TempDirOutput

instance HasCodec TempDirOutput where
  codec =
    object "TempDirOutput" $
      TempDirOutput
        <$> requiredFieldWith "path" (bimapCodec (left show . parseAbsDir) fromAbsDir codec) "dir path" .= tempDirOutputPath

tempDirProvider :: Provider TempDirSpecification TempDirOutput
tempDirProvider =
  Provider
    { providerName = "temporary-directory",
      providerQuery = queryTempDir,
      providerApply = applyTempDir,
      providerCheck = checkTempDir,
      providerDestroy = destroyTempDir
    }

queryTempDir :: ResourceName -> P (QueryResult TempDirOutput)
queryTempDir resourceName = do
  path <- resolveResourceTempDir resourceName
  liftIO $ do
    exists <- doesDirExist path
    let output = TempDirOutput {tempDirOutputPath = path}
    pure $
      QuerySuccess $
        if exists
          then ExistsRemotely output
          else DoesNotExistRemotely

applyTempDir :: ResourceName -> TempDirSpecification -> P (ApplyResult TempDirOutput)
applyTempDir resourceName TempDirSpecification = do
  path <- resolveResourceTempDir resourceName
  exists <- liftIO $ doesDirExist path
  when (not exists) $ makeTempDir path
  let output = TempDirOutput {tempDirOutputPath = path}
  pure $ ApplySuccess output

checkTempDir :: ResourceName -> TempDirSpecification -> P (CheckResult TempDirOutput)
checkTempDir resourceName TempDirSpecification = do
  path <- resolveResourceTempDir resourceName
  exists <- liftIO $ doesDirExist path
  if exists
    then pure $ CheckSuccess TempDirOutput {tempDirOutputPath = path}
    else fail "Directory does not exist."

destroyTempDir :: ResourceName -> P DestroyResult
destroyTempDir resourceName = do
  path <- resolveResourceTempDir resourceName
  removeTempDir path
  pure DestroySuccess

removeTempDir :: Path Abs Dir -> P ()
removeTempDir path = do
  logDebugN $ T.pack $ unwords ["Removing", show path]
  liftIO $ ignoringAbsence $ removeDirRecur path

makeTempDir :: Path Abs Dir -> P ()
makeTempDir path = do
  logDebugN $ T.pack $ unwords ["Creating", show path]
  liftIO $ ensureDir path

resolveResourceTempDir :: ResourceName -> P (Path Abs Dir)
resolveResourceTempDir resourceName = do
  path <- liftIO $ parseAbsDir $ "/tmp/declops-" <> T.unpack (unResourceName resourceName)
  logDebugN $ T.pack $ unwords ["Resolving temporary dir to path:", show path]
  pure path
