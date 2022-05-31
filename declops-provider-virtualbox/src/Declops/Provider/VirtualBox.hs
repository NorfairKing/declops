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
  { virtualBoxSpecificationBaseFolder :: !(Path Abs Dir)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxSpecification)

instance Validity VirtualBoxSpecification

instance HasCodec VirtualBoxSpecification where
  codec =
    object "VirtualBoxSpecification" $
      VirtualBoxSpecification
        <$> requiredField "base-folder" "base folder" .= virtualBoxSpecificationBaseFolder

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

queryVirtualBox :: ResourceName -> UUID -> P (QueryResult VirtualBoxOutput)
queryVirtualBox resourceName uuid = do
  mTups <- getVMInfo uuid
  case mTups of
    Nothing -> pure $ QuerySuccess DoesNotExistRemotely
    Just VirtualBoxInfo {..} ->
      pure $
        QuerySuccess $
          ExistsRemotely $
            VirtualBoxOutput
              { virtualBoxOutputUUID = virtualBoxInfoUUID,
                virtualBoxOutputSettingsFile = virtualBoxInfoSettingsFile
              }

applyVirtualBox ::
  ResourceName ->
  VirtualBoxSpecification ->
  ApplyContext UUID VirtualBoxOutput ->
  P (ApplyResult UUID VirtualBoxOutput)
applyVirtualBox resourceName specification@VirtualBoxSpecification {..} applyContext = do
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      logDebugN "Creating a brand new VM."
      (uuid, settingsFile) <- makeVirtualBox resourceName virtualBoxSpecificationBaseFolder Nothing
      let output = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = settingsFile}
      pure $ ApplySuccess uuid output
    ExistsLocallyButNotRemotely uuid -> do
      logDebugN $ T.pack $ unwords ["VM with this UUID vanished, creating a new one with the same uuid:", UUID.toString uuid]

      -- Make sure there's no settings file in the way.
      predictedSettingsFile <- predictSettionsFile resourceName virtualBoxSpecificationBaseFolder
      settingsFileAlreadyExists <- doesFileExist predictedSettingsFile
      when settingsFileAlreadyExists $ do
        logDebugN "Settings file already existed, even if the vm has vanished, so removing it."
        liftIO $ ignoringAbsence $ removeFile predictedSettingsFile

      (uuid, settingsFile) <- makeVirtualBox resourceName virtualBoxSpecificationBaseFolder (Just uuid)
      let output = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = settingsFile}
      pure $ ApplySuccess uuid output
    ExistsLocallyAndRemotely uuid output -> do
      logDebugN $
        T.pack $
          unwords
            [ "VM already exists, checking whether it is already deployed correctly:",
              show uuid
            ]
      mVMInfo <- getVMInfo uuid
      case mVMInfo of
        Nothing -> fail "Should have been able to find the VM info"
        Just VirtualBoxInfo {..} -> do
          if isProperPrefixOf virtualBoxSpecificationBaseFolder virtualBoxInfoSettingsFile
            then do
              logDebugN "VM is already deployed correctly, leaving it as-is."
              pure $ ApplySuccess uuid output
            else do
              logDebugN "VM is deployed a different settings file, recreating it."
              unregisterVirtualBox resourceName uuid
              (uuid, settingsFile) <- makeVirtualBox resourceName virtualBoxSpecificationBaseFolder Nothing
              let newOutput = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = settingsFile}
              pure $ ApplySuccess uuid newOutput

checkVirtualBox :: ResourceName -> VirtualBoxSpecification -> UUID -> P (CheckResult VirtualBoxOutput)
checkVirtualBox resourceName VirtualBoxSpecification {..} uuid = do
  mTups <- getVMInfo uuid
  case mTups of
    Nothing -> fail $ unwords ["VM does not exist:", show uuid]
    Just VirtualBoxInfo {..} -> do
      if virtualBoxInfoUUID == uuid
        then do
          if isProperPrefixOf virtualBoxSpecificationBaseFolder virtualBoxInfoSettingsFile
            then
              pure $
                CheckSuccess $
                  VirtualBoxOutput
                    { virtualBoxOutputUUID = uuid,
                      virtualBoxOutputSettingsFile = virtualBoxInfoSettingsFile
                    }
            else
              fail $
                unlines
                  [ "The settings file is not in the base folder",
                    unwords
                      [ "Settings file:",
                        show virtualBoxInfoSettingsFile
                      ],
                    unwords
                      [ "Base folder:",
                        show virtualBoxSpecificationBaseFolder
                      ]
                  ]
        else
          fail $
            unlines
              [ "UUID does not match the reference:",
                unwords ["reference:", show uuid],
                unwords ["actual:", show virtualBoxInfoUUID]
              ]

destroyVirtualBox :: ResourceName -> UUID -> P DestroyResult
destroyVirtualBox resourceName uuid = do
  mVmInfo <- getVMInfo uuid
  case mVmInfo of
    Nothing -> pure DestroySuccess
    Just _ -> unregisterVirtualBox resourceName uuid

unregisterVirtualBox :: ResourceName -> UUID -> P DestroyResult
unregisterVirtualBox resourceName uuid = do
  logDebugN $ T.pack $ unwords ["Unregistering virtualbox with uuid", UUID.toString uuid]
  let pc =
        proc
          "VBoxManage"
          [ "unregistervm",
            UUID.toString uuid,
            "--delete"
          ]

  logProcessConfig pc
  ec <- runProcess pc
  case ec of
    ExitSuccess -> pure DestroySuccess
    ExitFailure c -> fail $ unwords ["VBoxManage unregister failed with exit code:", show c]

getVMInfo :: UUID -> P (Maybe VirtualBoxInfo)
getVMInfo uuid = do
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
      let requireVal key = case lookup key tups of
            Nothing ->
              throwP $
                ProviderException $
                  unlines $
                    unwords ["Expected to have found this key in the vminfo output:", show key] :
                    map show tups
            Just val -> pure val
      virtualBoxInfoUUID <- case lookup "UUID" tups of
        Nothing -> liftIO $ die "uuid not found." -- TODO
        Just uuidVal -> case UUID.fromString $ read $ T.unpack uuidVal of
          Nothing -> liftIO $ die $ unwords ["uuid not readable:", show uuidVal]
          Just uuid -> pure uuid
      virtualBoxInfoSettingsFile <- case lookup "CfgFile" tups of
        Nothing -> liftIO $ die "settings file not found."
        Just settingsFileVal -> case readMaybe (T.unpack settingsFileVal) >>= parseAbsFile of
          Nothing -> liftIO $ die $ unwords ["settings file not readable:", show settingsFileVal]
          Just settingsFile -> pure settingsFile
      pure $ Just $ VirtualBoxInfo {..}
    ExitFailure _ -> pure Nothing

data VirtualBoxInfo = VirtualBoxInfo
  { virtualBoxInfoUUID :: !UUID,
    virtualBoxInfoSettingsFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

makeVirtualBox :: ResourceName -> Path Abs Dir -> Maybe UUID -> P (UUID, Path Abs File)
makeVirtualBox resourceName baseFolder mUuid = do
  logDebugN "Creating the VM settings file."
  let pc =
        proc
          "VBoxManage"
          [ "createvm",
            "--name",
            T.unpack $ resourceNameText resourceName,
            "--ostype",
            "Linux_64",
            "--basefolder",
            fromAbsDir baseFolder,
            "--register"
          ]

  logProcessConfig pc
  (ec, output) <- readProcessStdout pc
  case ec of
    ExitFailure c -> fail $ unwords ["createvm failed with exit code:", show c]
    ExitSuccess -> do
      let outputText = TE.decodeUtf8With TE.lenientDecode $ LB.toStrict output
      logDebugN $ T.pack $ unlines ["Read stdout:", T.unpack outputText]
      let tups = flip mapMaybe (T.lines outputText) $ \t ->
            case T.splitOn ": " t of
              [name, val] -> Just (name, val)
              _ -> Nothing
      let requireVal key = case lookup key tups of
            Nothing ->
              fail $
                unlines $
                  unwords ["Expected to have found this key in the createvm output:", show key] :
                  map show tups
            Just val -> pure val

      uuidVal <- requireVal "UUID"
      uuid <- case UUID.fromText uuidVal of
        Nothing -> fail $ unwords ["Could not parse uuid:", show uuidVal]
        Just u -> pure u
      logDebugN $ T.pack $ unwords ["VM was assigned uuid: ", show uuid]
      settingsFileVal <- requireVal "Settings file"
      settingsFile <- resolveFile' $ read $ T.unpack $ T.replace "'" "\"" settingsFileVal
      logDebugN $ T.pack $ unwords ["VM was assigned settings file: ", show settingsFile]
      pure (uuid, settingsFile)

predictSettionsFile :: ResourceName -> Path Abs Dir -> P (Path Abs File)
predictSettionsFile resourceName baseFolder = do
  settingsFile <-
    resolveFile baseFolder $
      let n = T.unpack $ resourceNameText resourceName
       in concat [n, "/", n, ".vbox"]
  logDebugN $ T.pack $ unwords ["Predicting that the settings file will be at", fromAbsFile settingsFile]
  pure settingsFile

logProcessConfig :: ProcessConfig input output error -> P ()
logProcessConfig pc = logDebugN $ T.pack $ show pc
