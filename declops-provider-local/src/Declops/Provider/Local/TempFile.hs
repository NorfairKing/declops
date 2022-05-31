{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempFile where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List
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

tempFileProvider :: Provider TempFileSpecification (Path Abs File) TempFileOutput
tempFileProvider =
  Provider
    { providerName = "temporary-file",
      providerQuery = queryTempFile,
      providerApply = applyTempFile,
      providerCheck = checkTempFile,
      providerDestroy = destroyTempFile
    }

queryTempFile :: ResourceName -> Path Abs File -> P (QueryResult TempFileOutput)
queryTempFile _ path = liftIO $ do
  mContents <- forgivingAbsence $ TIO.readFile $ fromAbsFile path
  pure $
    QuerySuccess $ case mContents of
      Nothing -> DoesNotExistRemotely
      Just contents ->
        ExistsRemotely
          TempFileOutput
            { tempFileOutputPath = path,
              tempFileOutputContents = contents
            }

applyTempFile :: ResourceName -> TempFileSpecification -> ApplyContext (Path Abs File) TempFileOutput -> P (ApplyResult (Path Abs File) TempFileOutput)
applyTempFile _ TempFileSpecification {..} applyContext = liftIO $ do
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      tfile <- makeTempFile tempFileSpecificationBase tempFileSpecificationTemplate tempFileSpecificationContents
      let output = TempFileOutput {tempFileOutputPath = tfile, tempFileOutputContents = tempFileSpecificationContents}
      pure $ ApplySuccess tfile output
    ExistsLocallyButNotRemotely reference -> do
      removeTempFile reference
      tfile <- makeTempFile tempFileSpecificationBase tempFileSpecificationTemplate tempFileSpecificationContents
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
          removeTempFile reference
          tfile <- makeTempFile tempFileSpecificationBase tempFileSpecificationTemplate tempFileSpecificationContents
          let newOutput = TempFileOutput {tempFileOutputPath = tfile, tempFileOutputContents = tempFileSpecificationContents}
          pure $ ApplySuccess tfile newOutput

checkTempFile :: ResourceName -> TempFileSpecification -> Path Abs File -> P (CheckResult TempFileOutput)
checkTempFile _ TempFileSpecification {..} path = liftIO $ do
  case stripProperPrefix tempFileSpecificationBase path of
    Nothing -> pure $ CheckFailure "File had the wrong base."
    Just subfile ->
      if T.unpack tempFileSpecificationTemplate `isInfixOf` fromRelFile subfile
        then do
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
        else
          fail $
            unlines
              [ "File did not have the right template:",
                unwords ["expected:   ", T.unpack tempFileSpecificationTemplate],
                unwords ["actual file:", fromAbsFile path]
              ]

destroyTempFile :: ResourceName -> Path Abs File -> P DestroyResult
destroyTempFile _ path = liftIO $ do
  removeTempFile path
  pure DestroySuccess

removeTempFile :: Path Abs File -> IO ()
removeTempFile path = ignoringAbsence $ removeFile path

makeTempFile :: Path Abs Dir -> Text -> Text -> IO (Path Abs File)
makeTempFile base template contents = do
  (tfile, handle) <- openTempFile base (T.unpack template)
  TIO.hPutStr handle contents
  hClose handle
  pure tfile
