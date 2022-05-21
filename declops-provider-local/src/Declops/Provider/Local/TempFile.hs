{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempFile where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Declops.Provider
import Declops.Provider.Local.TempDir ()
import GHC.Generics (Generic)
import Path
import Path.IO
import System.IO (hClose)

data TempFileSpecification = TempFileSpecification
  { tempFileSpecificationBase :: !(Path Abs Dir),
    tempFileSpecificationTemplate :: !Text, -- Text because of json roundtrip requirement
    tempFileSpecificationContents :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempFileSpecification)

instance Validity TempFileSpecification

instance HasCodec TempFileSpecification where
  codec =
    object "TempFileSpecification" $
      TempFileSpecification
        <$> requiredField "base" "base dir" .= tempFileSpecificationBase
        <*> requiredField "template" "template file name" .= tempFileSpecificationTemplate
        <*> requiredField "contents" "file contents" .= tempFileSpecificationContents

data TempFileOutput = TempFileOutput
  { tempFileOutputPath :: !(Path Abs File),
    tempFileOutputContents :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempFileOutput)

instance Validity TempFileOutput

instance HasCodec TempFileOutput where
  codec =
    object "TempFileOutput" $
      TempFileOutput
        <$> requiredField "path" "file path" .= tempFileOutputPath
        <*> requiredField "contents" "file contents" .= tempFileOutputContents

instance HasCodec (Path Abs File) where
  codec = bimapCodec (left show . parseAbsFile) fromAbsFile codec

tempFileProvider :: Provider TempFileSpecification (Path Abs File) TempFileOutput
tempFileProvider =
  Provider
    { providerName = "temporary-file",
      providerQuery = \reference -> do
        mContents <- forgivingAbsence $ TIO.readFile $ fromAbsFile reference
        pure $ case mContents of
          Nothing -> DoesNotExistRemotely
          Just contents ->
            ExistsRemotely
              TempFileOutput
                { tempFileOutputPath = reference,
                  tempFileOutputContents = contents
                },
      providerApply = \TempFileSpecification {..} applyContext -> do
        case applyContext of
          DoesNotExistLocallyNorRemotely -> do
            (tfile, handle) <- openTempFile tempFileSpecificationBase (T.unpack tempFileSpecificationTemplate)
            TIO.hPutStr handle tempFileSpecificationContents
            hClose handle
            let output = TempFileOutput {tempFileOutputPath = tfile, tempFileOutputContents = tempFileSpecificationContents}
            pure $ ApplySuccess tfile output
          ExistsLocallyButNotRemotely reference -> do
            ignoringAbsence $ removeFile reference
            (tfile, handle) <- openTempFile tempFileSpecificationBase (T.unpack tempFileSpecificationTemplate)
            TIO.hPutStr handle tempFileSpecificationContents
            hClose handle
            let output = TempFileOutput {tempFileOutputPath = tfile, tempFileOutputContents = tempFileSpecificationContents}
            pure $ ApplySuccess tfile output
          ExistsLocallyAndRemotely reference output@TempFileOutput {..} -> do
            let alreadyCorrect =
                  and
                    [ case stripProperPrefix tempFileSpecificationBase tempFileOutputPath of
                        Nothing -> False
                        Just subfile -> T.unpack tempFileSpecificationTemplate `isInfixOf` fromRelFile subfile,
                      tempFileOutputContents == tempFileSpecificationContents
                    ]
            if alreadyCorrect
              then pure $ ApplySuccess reference output
              else do
                ignoringAbsence $ removeFile reference
                (tfile, handle) <- openTempFile tempFileSpecificationBase (T.unpack tempFileSpecificationTemplate)
                TIO.hPutStr handle tempFileSpecificationContents
                hClose handle
                let newOutput = TempFileOutput {tempFileOutputPath = tfile, tempFileOutputContents = tempFileSpecificationContents}
                pure $ ApplySuccess tfile newOutput,
      providerCheck = \TempFileSpecification {..} reference -> do
        case stripProperPrefix tempFileSpecificationBase reference of
          Nothing -> pure $ CheckFailure "File had the wrong base."
          Just subfile ->
            if T.unpack tempFileSpecificationTemplate `isInfixOf` fromRelFile subfile
              then do
                mContents <- forgivingAbsence $ TIO.readFile $ fromAbsFile reference
                pure $ case mContents of
                  Nothing -> CheckFailure "File does not exist."
                  Just contents ->
                    if contents == tempFileSpecificationContents
                      then
                        CheckSuccess
                          TempFileOutput
                            { tempFileOutputPath = reference,
                              tempFileOutputContents = contents
                            }
                      else
                        CheckFailure $
                          unlines
                            [ "File did not have the right contents:",
                              unwords ["expected:", show tempFileSpecificationContents],
                              unwords ["actual:  ", show contents]
                            ]
              else
                pure $
                  CheckFailure $
                    unlines
                      [ "File did not have the right template:",
                        unwords ["expected:   ", T.unpack tempFileSpecificationTemplate],
                        unwords ["actual file:", fromAbsFile reference]
                      ],
      providerDestroy = \reference -> do
        ignoringAbsence $ removeFile reference
        pure DestroySuccess
    }
