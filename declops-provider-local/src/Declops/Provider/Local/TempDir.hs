{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempDir where

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
      providerQuery = \reference -> do
        exists <- doesDirExist reference
        let output = TempDirOutput {tempDirOutputPath = reference}
        pure $
          if exists
            then ExistsRemotely output
            else DoesNotExistRemotely,
      providerApply = \TempDirSpecification {..} applyContext -> do
        case applyContext of
          DoesNotExistLocallyNorRemotely -> do
            tdir <- createTempDir tempDirSpecificationBase (T.unpack tempDirSpecificationTemplate)
            let output = TempDirOutput {tempDirOutputPath = tdir}
            pure $ ApplySuccess tdir output
          ExistsLocallyButNotRemotely reference -> do
            ignoringAbsence $ removeDirRecur reference
            tdir <- createTempDir tempDirSpecificationBase (T.unpack tempDirSpecificationTemplate)
            let output = TempDirOutput {tempDirOutputPath = tdir}
            pure $ ApplySuccess tdir output
          ExistsLocallyAndRemotely reference output@TempDirOutput {..} -> do
            let alreadyCorrect = case stripProperPrefix tempDirSpecificationBase tempDirOutputPath of
                  Nothing -> False
                  Just subdir -> T.unpack tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
            if alreadyCorrect
              then pure $ ApplySuccess reference output
              else do
                ignoringAbsence $ removeDirRecur reference
                tdir <- createTempDir tempDirSpecificationBase (T.unpack tempDirSpecificationTemplate)
                let newOutput = TempDirOutput {tempDirOutputPath = tdir}
                pure $ ApplySuccess tdir newOutput,
      providerCheck = \TempDirSpecification {..} reference -> do
        exists <- doesDirExist reference
        pure $
          if exists
            then case stripProperPrefix tempDirSpecificationBase reference of
              Nothing -> CheckFailure "Directory had the wrong base."
              Just subdir ->
                if T.unpack tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
                  then CheckSuccess TempDirOutput {tempDirOutputPath = reference}
                  else
                    CheckFailure $
                      unlines
                        [ "Directory did not have the right template:",
                          unwords ["expected:  ", T.unpack tempDirSpecificationTemplate],
                          unwords ["actual dir:", fromAbsDir reference]
                        ]
            else CheckFailure "Directory does not exist.",
      providerDestroy = \reference -> do
        ignoringAbsence $ removeDirRecur reference
        pure DestroySuccess
    }
