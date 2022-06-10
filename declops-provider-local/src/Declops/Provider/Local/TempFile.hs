{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempFile where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO

data TempFileSpecification = TempFileSpecification
  { tempFileSpecificationContents :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TempFileSpecification)

instance Validity TempFileSpecification

instance HasCodec TempFileSpecification where
  codec =
    object "TempFileSpecification" $
      TempFileSpecification
        <$> requiredField "contents" "file contents" .= tempFileSpecificationContents

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

tempFileProvider :: Provider TempFileSpecification () TempFileOutput
tempFileProvider =
  Provider
    { providerName = "temporary-file",
      providerQuery = queryTempFile,
      providerApply = applyTempFile,
      providerCheck = checkTempFile,
      providerDestroy = destroyTempFile
    }

queryTempFile :: ResourceName -> () -> P (QueryResult TempFileOutput)
queryTempFile resourceName () = do
  path <- resolveResourceTempFile resourceName
  mContents <- liftIO $ forgivingAbsence $ TIO.readFile $ fromAbsFile path
  pure $
    QuerySuccess $ case mContents of
      Nothing -> DoesNotExistRemotely
      Just contents ->
        ExistsRemotely
          TempFileOutput
            { tempFileOutputPath = path,
              tempFileOutputContents = contents
            }

applyTempFile :: ResourceName -> TempFileSpecification -> ApplyContext () TempFileOutput -> P (ApplyResult () TempFileOutput)
applyTempFile resourceName TempFileSpecification {..} applyContext = do
  path <- resolveResourceTempFile resourceName
  let output = TempFileOutput {tempFileOutputPath = path, tempFileOutputContents = tempFileSpecificationContents}
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      makeTempFile path tempFileSpecificationContents
      pure $ ApplySuccess () output
    ExistsLocallyButNotRemotely () -> do
      makeTempFile path tempFileSpecificationContents
      pure $ ApplySuccess () output
    ExistsLocallyAndRemotely () TempFileOutput {..} -> do
      let alreadyCorrect =
            and
              [ tempFileOutputPath == path,
                tempFileOutputContents == tempFileSpecificationContents
              ]
      if alreadyCorrect
        then pure $ ApplySuccess () output
        else do
          removeTempFile tempFileOutputPath
          makeTempFile path tempFileSpecificationContents
          pure $ ApplySuccess () output

checkTempFile :: ResourceName -> TempFileSpecification -> () -> P (CheckResult TempFileOutput)
checkTempFile resourceName TempFileSpecification {..} () = do
  path <- resolveResourceTempFile resourceName
  liftIO $ do
    mContents <- forgivingAbsence $ TIO.readFile $ fromAbsFile path
    case mContents of
      Nothing -> fail "File does not exist."
      Just contents ->
        if contents == tempFileSpecificationContents
          then
            pure $
              CheckSuccess
                TempFileOutput
                  { tempFileOutputPath = path,
                    tempFileOutputContents = contents
                  }
          else
            fail $
              unlines
                [ "File did not have the right contents:",
                  unwords ["expected:", show tempFileSpecificationContents],
                  unwords ["actual:  ", show contents]
                ]

destroyTempFile :: ResourceName -> () -> P DestroyResult
destroyTempFile resourceName () = do
  path <- resolveResourceTempFile resourceName
  removeTempFile path
  pure DestroySuccess

removeTempFile :: Path Abs File -> P ()
removeTempFile path = do
  logDebugN $ T.pack $ unwords ["Removing", show path]
  liftIO $ ignoringAbsence $ removeFile path

makeTempFile :: Path Abs File -> Text -> P ()
makeTempFile path contents = do
  logDebugN $ T.pack $ unwords ["Creating", show path]
  liftIO $ TIO.writeFile (fromAbsFile path) contents

resolveResourceTempFile :: ResourceName -> P (Path Abs File)
resolveResourceTempFile resourceName = do
  path <- liftIO $ parseAbsFile $ "/tmp/declops-" <> T.unpack (unResourceName resourceName)
  logDebugN $ T.pack $ unwords ["Resolving temporary file to path:", show path]
  pure path
