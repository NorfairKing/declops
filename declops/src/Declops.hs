{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops where

import Data.List
import Data.Proxy
import Data.Text (Text)
import Data.Validity
import Data.Validity.Path
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

-- | A provider for a resource.
--
-- A provider has three type parameters:
--
-- * An input type, to declaratively specify what the resource should look like.
-- * A reference type, to refer to the resource in the local declops database.
-- * An output type, to contain all the information about the remote resource.
--
-- In this context "local" means "wherever declops is run" and "remote" means "in reality".
--
-- A provider contains:
--
-- * A name, to reference it.
-- * A query function, to find out if the resource with the given local reference still exists remotely.
-- * An apply function, to apply the current input to reality
-- * A check function, to find out if the remote resource still looks like what it should and works as it should.
-- * A destroy function, to destroy a resource
--
-- Each of these functions MUST be idempotent so that they can be retried.
-- Getting them all right is not an easy thing to do, which is why we provide a test suite.
data Provider input reference output = Provider
  { providerName :: !Text,
    providerQuery :: !(reference -> IO (RemoteState output)),
    providerApply :: !(input -> ApplyContext reference output -> IO (ApplyResult reference output)),
    providerCheck :: !(input -> output -> IO CheckResult),
    providerDestroy :: !(reference -> RemoteState output -> IO DestroyResult)
  }
  deriving (Generic)

data LocalState reference
  = DoesNotExistLocally
  | ExistsLocally !reference
  deriving (Show, Eq, Generic)

data RemoteState output
  = DoesNotExistRemotely
  | ExistsRemotely !output
  deriving (Show, Eq, Generic)

data ApplyContext reference output
  = DoesNotExistLocallyNorRemotely
  | ExistsLocallyButNotRemotely !reference
  | ExistsLocallyAndRemotely !reference !output
  deriving (Show, Eq, Generic)

data ApplyResult reference output
  = ApplySuccess !reference !output
  | ApplyFailure !String
  deriving (Show, Eq, Generic)

data CheckResult
  = CheckSuccess
  | CheckFailure !String
  deriving (Show, Eq, Generic)

data DestroyResult
  = DestroySuccess
  | DestroyFailure !String
  deriving (Show, Eq, Generic)
