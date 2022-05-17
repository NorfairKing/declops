{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops where

import Data.List
import Data.Validity
import Data.Validity.Path ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO

declopsMain :: IO ()
declopsMain = putStrLn "someFunc"

data TempDirInputs = TempDirInputs
  { tempDirInputBase :: !(Path Abs Dir),
    tempDirInputTemplate :: !FilePath
  }
  deriving (Show, Eq, Generic)

instance Validity TempDirInputs

tempDirProvider :: Provider TempDirInputs (Path Abs Dir) (Path Abs Dir)
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
      providerApply = \TempDirInputs {..} applyContext -> do
        case applyContext of
          DoesNotExistLocallyNorRemotely -> do
            tdir <- createTempDir tempDirInputBase tempDirInputTemplate
            pure $ ApplySuccess tdir tdir
          ExistsLocallyButNotRemotely _ -> do
            tdir <- createTempDir tempDirInputBase tempDirInputTemplate
            pure $ ApplySuccess tdir tdir
          ExistsLocallyAndRemotely reference remoteDir -> do
            let alreadyCorrect = case stripProperPrefix tempDirInputBase remoteDir of
                  Nothing -> False
                  Just subdir -> tempDirInputTemplate `isInfixOf` fromRelDir subdir
            if alreadyCorrect
              then pure $ ApplySuccess reference remoteDir
              else do
                tdir <- createTempDir tempDirInputBase tempDirInputTemplate
                pure $ ApplySuccess tdir tdir,
      providerCheck = \input output -> do
        exists <- doesDirExist output
        pure $
          if exists
            then case stripProperPrefix (tempDirInputBase input) output of
              Nothing -> CheckFailure "Directory had the wrong base."
              Just subdir ->
                if tempDirInputTemplate input `isInfixOf` fromRelDir subdir
                  then CheckSuccess
                  else
                    CheckFailure $
                      unlines
                        [ "Directory did not have the right template:",
                          unwords ["expected:", tempDirInputTemplate input],
                          unwords ["actual dir:", fromAbsDir output]
                        ]
            else CheckFailure "Directory does not exist.",
      providerDestroy = \reference _ -> do
        ignoringAbsence $ removeDir reference
        pure DestroySuccess
    }
