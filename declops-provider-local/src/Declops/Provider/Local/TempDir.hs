{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempDir
  ( TempDirSpecification (..),
    TempDirOutput (..),
    tempDirProvider,
  )
where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO

data TempDirSpecification = TempDirSpecification
  { tempDirSpecificationBase :: !(Path Abs Dir),
    tempDirSpecificationTemplate :: !Text -- Text because of JSON roundtrip requirement
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempDirSpecification)

instance Validity TempDirSpecification

instance HasCodec TempDirSpecification where
  codec =
    object "TempDirSpecification" $
      TempDirSpecification
        <$> requiredField "base" "base directory" .= tempDirSpecificationBase
        <*> requiredField "template" "template directory name" .= tempDirSpecificationTemplate

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

tempDirProvider :: Provider TempDirSpecification (Path Abs Dir) TempDirOutput
tempDirProvider =
  Provider
    { providerName = "temporary-directory",
      providerQuery = queryTempDir,
      providerApply = applyTempDir,
      providerCheck = checkTempDir,
      providerDestroy = destroyTempDir
    }

queryTempDir :: Path Abs Dir -> IO (RemoteState TempDirOutput)
queryTempDir path = do
  exists <- doesDirExist path
  let output = TempDirOutput {tempDirOutputPath = path}
  pure $
    if exists
      then ExistsRemotely output
      else DoesNotExistRemotely

applyTempDir :: TempDirSpecification -> ApplyContext (Path Abs Dir) TempDirOutput -> IO (ApplyResult (Path Abs Dir) TempDirOutput)
applyTempDir TempDirSpecification {..} applyContext = do
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      tdir <- createTempDir tempDirSpecificationBase (T.unpack tempDirSpecificationTemplate)
      let output = TempDirOutput {tempDirOutputPath = tdir}
      pure $ ApplySuccess tdir output
    ExistsLocallyButNotRemotely path -> do
      ignoringAbsence $ removeDirRecur path
      tdir <- createTempDir tempDirSpecificationBase (T.unpack tempDirSpecificationTemplate)
      let output = TempDirOutput {tempDirOutputPath = tdir}
      pure $ ApplySuccess tdir output
    ExistsLocallyAndRemotely path output@TempDirOutput {..} -> do
      let alreadyCorrect = case stripProperPrefix tempDirSpecificationBase tempDirOutputPath of
            Nothing -> False
            Just subdir -> T.unpack tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
      if alreadyCorrect
        then pure $ ApplySuccess path output
        else do
          ignoringAbsence $ removeDirRecur path
          tdir <- createTempDir tempDirSpecificationBase (T.unpack tempDirSpecificationTemplate)
          let newOutput = TempDirOutput {tempDirOutputPath = tdir}
          pure $ ApplySuccess tdir newOutput

checkTempDir :: TempDirSpecification -> Path Abs Dir -> IO (CheckResult TempDirOutput)
checkTempDir TempDirSpecification {..} path = do
  exists <- doesDirExist path
  pure $
    if exists
      then case stripProperPrefix tempDirSpecificationBase path of
        Nothing -> CheckFailure "Directory had the wrong base."
        Just subdir ->
          if T.unpack tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
            then CheckSuccess TempDirOutput {tempDirOutputPath = path}
            else
              CheckFailure $
                unlines
                  [ "Directory did not have the right template:",
                    unwords ["expected:  ", T.unpack tempDirSpecificationTemplate],
                    unwords ["actual dir:", fromAbsDir path]
                  ]
      else CheckFailure "Directory does not exist."

destroyTempDir :: Path Abs Dir -> IO DestroyResult
destroyTempDir path = do
  ignoringAbsence $ removeDirRecur path
  pure DestroySuccess
