{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Provider.TempDir where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.List
import Data.Validity
import Data.Validity.Path ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO

data TempDirSpecification = TempDirSpecification
  { tempDirSpecificationBase :: !(Path Abs Dir),
    tempDirSpecificationTemplate :: !FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempDirSpecification)

instance Validity TempDirSpecification

instance HasCodec TempDirSpecification where
  codec =
    object "TempDirSpecification" $
      TempDirSpecification
        <$> requiredFieldWith "base" (bimapCodec (left show . parseAbsDir) fromAbsDir codec) "base directory" .= tempDirSpecificationBase
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
            tdir <- createTempDir tempDirSpecificationBase tempDirSpecificationTemplate
            let output = TempDirOutput {tempDirOutputPath = tdir}
            pure $ ApplySuccess tdir output
          ExistsLocallyButNotRemotely reference -> do
            ignoringAbsence $ removeDir reference
            tdir <- createTempDir tempDirSpecificationBase tempDirSpecificationTemplate
            let output = TempDirOutput {tempDirOutputPath = tdir}
            pure $ ApplySuccess tdir output
          ExistsLocallyAndRemotely reference output@TempDirOutput {..} -> do
            let alreadyCorrect = case stripProperPrefix tempDirSpecificationBase tempDirOutputPath of
                  Nothing -> False
                  Just subdir -> tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
            if alreadyCorrect
              then pure $ ApplySuccess reference output
              else do
                ignoringAbsence $ removeDir reference
                tdir <- createTempDir tempDirSpecificationBase tempDirSpecificationTemplate
                let newOutput = TempDirOutput {tempDirOutputPath = tdir}
                pure $ ApplySuccess tdir newOutput,
      providerCheck = \TempDirSpecification {..} reference -> do
        exists <- doesDirExist reference
        pure $
          if exists
            then case stripProperPrefix tempDirSpecificationBase reference of
              Nothing -> CheckFailure "Directory had the wrong base."
              Just subdir ->
                if tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
                  then CheckSuccess
                  else
                    CheckFailure $
                      unlines
                        [ "Directory did not have the right template:",
                          unwords ["expected:  ", tempDirSpecificationTemplate],
                          unwords ["actual dir:", fromAbsDir reference]
                        ]
            else CheckFailure "Directory does not exist.",
      providerDestroy = \reference -> do
        ignoringAbsence $ removeDir reference
        pure DestroySuccess
    }
