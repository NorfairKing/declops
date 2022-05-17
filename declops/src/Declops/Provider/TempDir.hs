{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Provider.TempDir where

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
  deriving (Show, Eq, Generic)

instance Validity TempDirSpecification

tempDirProvider :: Provider TempDirSpecification (Path Abs Dir) (Path Abs Dir)
tempDirProvider =
  Provider
    { providerName = "temporary-directory",
      providerQuery = \reference -> do
        exists <- doesDirExist reference
        let output = reference
        pure $
          if exists
            then ExistsRemotely output
            else DoesNotExistRemotely,
      providerApply = \TempDirSpecification {..} applyContext -> do
        case applyContext of
          DoesNotExistLocallyNorRemotely -> do
            tdir <- createTempDir tempDirSpecificationBase tempDirSpecificationTemplate
            pure $ ApplySuccess tdir tdir
          ExistsLocallyButNotRemotely _ -> do
            tdir <- createTempDir tempDirSpecificationBase tempDirSpecificationTemplate
            pure $ ApplySuccess tdir tdir
          ExistsLocallyAndRemotely reference remoteDir -> do
            let alreadyCorrect = case stripProperPrefix tempDirSpecificationBase remoteDir of
                  Nothing -> False
                  Just subdir -> tempDirSpecificationTemplate `isInfixOf` fromRelDir subdir
            if alreadyCorrect
              then pure $ ApplySuccess reference remoteDir
              else do
                tdir <- createTempDir tempDirSpecificationBase tempDirSpecificationTemplate
                pure $ ApplySuccess tdir tdir,
      providerCheck = \specification output -> do
        exists <- doesDirExist output
        pure $
          if exists
            then case stripProperPrefix (tempDirSpecificationBase specification) output of
              Nothing -> CheckFailure "Directory had the wrong base."
              Just subdir ->
                if tempDirSpecificationTemplate specification `isInfixOf` fromRelDir subdir
                  then CheckSuccess
                  else
                    CheckFailure $
                      unlines
                        [ "Directory did not have the right template:",
                          unwords ["expected:", tempDirSpecificationTemplate specification],
                          unwords ["actual dir:", fromAbsDir output]
                        ]
            else CheckFailure "Directory does not exist.",
      providerDestroy = \reference _ -> do
        ignoringAbsence $ removeDir reference
        pure DestroySuccess
    }
