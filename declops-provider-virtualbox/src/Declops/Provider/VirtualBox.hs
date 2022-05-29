{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBox where

import Autodocodec
import Control.Arrow (left)
import Control.Exception
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as TIO
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.UUID ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO
import System.Exit
import System.IO (hClose)
import System.Process.Typed
import Text.Read

data VirtualBoxSpecification = VirtualBoxSpecification
  { virtualBoxSpecificationName :: !Text,
    virtualBoxSpecificationBaseFolder :: !(Path Abs Dir)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxSpecification)

instance Validity VirtualBoxSpecification where
  validate vbs@VirtualBoxSpecification {..} =
    mconcat
      [ genericValidate vbs,
        declare "The name is not empty" $ not $ T.null virtualBoxSpecificationName
      ]

instance HasCodec VirtualBoxSpecification where
  codec =
    object "VirtualBoxSpecification" $
      VirtualBoxSpecification
        <$> requiredField "name" "name" .= virtualBoxSpecificationName
        <*> requiredField "base-folder" "base folder" .= virtualBoxSpecificationBaseFolder

data VirtualBoxOutput = VirtualBoxOutput
  { virtualBoxOutputUUID :: !UUID,
    virtualBoxOutputSettingsFile :: !(Path Abs File)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxOutput)

instance Validity VirtualBoxOutput

instance HasCodec VirtualBoxOutput where
  codec =
    object "VirtualBoxOutput" $
      VirtualBoxOutput
        <$> requiredField "uuid" "vm uuid" .= virtualBoxOutputUUID
        <*> requiredField "settings-file" "path to the settings file" .= virtualBoxOutputSettingsFile

instance HasCodec UUID where
  codec = bimapCodec (maybe (Left "not a valid uuid") Right . UUID.fromText) UUID.toText codec

virtualBoxProvider ::
  Provider
    VirtualBoxSpecification
    UUID
    VirtualBoxOutput
virtualBoxProvider =
  Provider
    { providerName = "virtualbox",
      providerQuery = queryVirtualBox,
      providerApply = applyVirtualBox,
      providerCheck = checkVirtualBox,
      providerDestroy = destroyVirtualBox
    }

queryVirtualBox :: UUID -> P (QueryResult VirtualBoxOutput)
queryVirtualBox uuid = do
  mTups <- getVMInfoTuples uuid
  case mTups of
    Nothing -> pure $ QuerySuccess DoesNotExistRemotely
    Just tups -> do
      virtualBoxOutputUUID <- case lookup "UUID" tups of
        Nothing -> liftIO $ die "uuid not found." -- TODO
        Just uuidVal -> case UUID.fromString $ read $ T.unpack uuidVal of
          Nothing -> liftIO $ die $ unwords ["uuid not readable:", show uuidVal]
          Just uuid -> pure uuid
      virtualBoxOutputSettingsFile <- case lookup "CfgFile" tups of
        Nothing -> liftIO $ die "settings file not found."
        Just settingsFileVal -> case readMaybe (T.unpack settingsFileVal) >>= parseAbsFile of
          Nothing -> liftIO $ die $ unwords ["settings file not readable:", show settingsFileVal]
          Just settingsFile -> pure settingsFile
      pure $ QuerySuccess $ ExistsRemotely VirtualBoxOutput {..}

applyVirtualBox ::
  VirtualBoxSpecification ->
  ApplyContext UUID VirtualBoxOutput ->
  P (ApplyResult UUID VirtualBoxOutput)
applyVirtualBox VirtualBoxSpecification {..} applyContext = do
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      logDebugN "Creating a brand new VM."
      (uuid, settingsFile) <- makeVirtualBox virtualBoxSpecificationName virtualBoxSpecificationBaseFolder Nothing
      registerVirtualBox settingsFile
      let output = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = settingsFile}
      pure $ ApplySuccess uuid output
    ExistsLocallyButNotRemotely uuid -> do
      logDebugN $ T.pack $ unwords ["VM with this UUID vanished, creating a new one with the same uuid:", UUID.toString uuid]

      -- Make sure there's no settings file in the way.
      predictedSettingsFile <- predictSettionsFile virtualBoxSpecificationName virtualBoxSpecificationBaseFolder
      settingsFileAlreadyExists <- doesFileExist predictedSettingsFile
      when settingsFileAlreadyExists $ do
        logDebugN "Settings file already existed, even if the vm has vanished, so removing it."
        liftIO $ ignoringAbsence $ removeFile predictedSettingsFile

      (uuid, settingsFile) <- makeVirtualBox virtualBoxSpecificationName virtualBoxSpecificationBaseFolder (Just uuid)
      registerVirtualBox settingsFile
      let output = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = settingsFile}
      pure $ ApplySuccess uuid output
    ExistsLocallyAndRemotely uuid output -> do
      logDebugN $
        T.pack $
          unwords
            [ "VM already exists, checking whether it is already deployed correctly:",
              show uuid
            ]
      pure $ ApplySuccess uuid output

checkVirtualBox :: VirtualBoxSpecification -> UUID -> P (CheckResult VirtualBoxOutput)
checkVirtualBox VirtualBoxSpecification {..} uuid = do
  mTups <- getVMInfoTuples uuid
  case mTups of
    Nothing -> pure $ CheckFailure $ unwords ["VM does not exist:", show uuid]
    Just tups -> do
      let requireVal key = case lookup key tups of
            Nothing ->
              throwP $
                ApplyException $ -- TODO
                  unlines $
                    unwords ["Expected to have found this key in the vminfo output:", show key] :
                    map show tups
            Just val -> pure val
      uuidVal <- requireVal "UUID"
      if uuidVal == UUID.toText uuid
        then
          pure $
            CheckFailure $
              unlines
                [ "UUID does not match the reference:",
                  unwords ["reference:", show uuid],
                  unwords ["actual:", show uuidVal]
                ]
        else do
          settingsFileVal <- requireVal "CfgFile"
          if T.pack (fromAbsDir virtualBoxSpecificationBaseFolder) `T.isPrefixOf` T.pack (read (T.unpack settingsFileVal))
            then pure $ CheckSuccess undefined
            else
              pure $
                CheckFailure $
                  unlines
                    [ "The settings file is not in the base folder",
                      unwords
                        [ "Settings file:",
                          show settingsFileVal
                        ],
                      unwords
                        [ "Base folder:",
                          show virtualBoxSpecificationBaseFolder
                        ]
                    ]

destroyVirtualBox :: UUID -> P DestroyResult
destroyVirtualBox uuid = undefined

getVMInfoTuples :: UUID -> P (Maybe [(Text, Text)])
getVMInfoTuples uuid = do
  let pc =
        proc
          "VBoxManage"
          [ "showvminfo",
            UUID.toString uuid,
            "--details",
            "--machinereadable"
          ]
  logProcessConfig pc
  (ec, output) <- readProcessStdout pc

  case ec of
    ExitSuccess -> do
      let outputText = TE.decodeUtf8With TE.lenientDecode $ LB.toStrict output
      logDebugN $ T.pack $ unlines ["Read stdout:", T.unpack outputText]
      let tups = flip mapMaybe (T.lines outputText) $ \t ->
            case T.splitOn "=" t of
              [key, val] -> Just (key, val)
              _ -> Nothing
      pure $ Just tups
    ExitFailure _ -> pure Nothing

makeVirtualBox :: Text -> Path Abs Dir -> Maybe UUID -> P (UUID, Path Abs File)
makeVirtualBox name baseFolder mUuid = do
  logDebugN "Creating the VM settings file."
  let pc =
        proc
          "VBoxManage"
          [ "createvm",
            "--name",
            T.unpack name,
            "--ostype",
            "Linux_64",
            "--basefolder",
            fromAbsDir baseFolder
          ]

  logProcessConfig pc
  (ec, output) <- readProcessStdout pc
  case ec of
    ExitFailure c -> throwP $ ApplyException $ unwords ["createvm failed with exit code:", show c]
    ExitSuccess -> do
      let outputText = TE.decodeUtf8With TE.lenientDecode $ LB.toStrict output
      logDebugN $ T.pack $ unlines ["Read stdout:", T.unpack outputText]
      let tups = flip mapMaybe (T.lines outputText) $ \t ->
            case T.splitOn ": " t of
              [name, val] -> Just (name, val)
              _ -> Nothing
      let requireVal key = case lookup key tups of
            Nothing ->
              throwP $
                ApplyException $
                  unlines $
                    unwords ["Expected to have found this key in the createvm output:", show key] :
                    map show tups
            Just val -> pure val

      uuidVal <- requireVal "UUID"
      uuid <- case UUID.fromText uuidVal of
        Nothing -> throwP $ ApplyException $ unwords ["Could not parse uuid:", show uuidVal]
        Just u -> pure u
      logDebugN $ T.pack $ unwords ["VM was assigned uuid: ", show uuid]
      settingsFileVal <- requireVal "Settings file"
      settingsFile <- resolveFile' $ read $ T.unpack $ T.replace "'" "\"" settingsFileVal
      logDebugN $ T.pack $ unwords ["VM was assigned settings file: ", show settingsFile]
      pure (uuid, settingsFile)

predictSettionsFile :: Text -> Path Abs Dir -> P (Path Abs File)
predictSettionsFile name baseFolder = do
  settingsFile <-
    resolveFile baseFolder $
      let n = T.unpack name
       in concat [n, "/", n, ".vbox"]
  logDebugN $ T.pack $ unwords ["Predicting that the settings file will be at", fromAbsFile settingsFile]
  pure settingsFile

registerVirtualBox :: Path Abs File -> P ()
registerVirtualBox settingsFile = do
  logDebugN "Registering the VM."
  let pc = proc "VBoxManage" ["registervm", fromAbsFile settingsFile]
  logProcessConfig pc
  ec <- runProcess pc
  case ec of
    ExitFailure c -> throwP $ ApplyException $ unwords ["registervm failed with exit code:", show c]
    ExitSuccess -> pure ()

logProcessConfig :: ProcessConfig input output error -> P ()
logProcessConfig pc = logDebugN $ T.pack $ show pc
