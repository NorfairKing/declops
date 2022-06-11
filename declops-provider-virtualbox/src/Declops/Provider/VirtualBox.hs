{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBox where

import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
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
import Paths_declops_provider_virtualbox
import qualified System.Environment as System
import System.Exit
import System.Process.Typed
import Text.Read

data VirtualBoxSpecification = VirtualBoxSpecification
  { virtualBoxSpecificationRunning :: !Bool,
    virtualBoxSpecificationBaseFolder :: !(Path Abs Dir)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxSpecification)

instance Validity VirtualBoxSpecification

instance HasCodec VirtualBoxSpecification where
  codec =
    object "VirtualBoxSpecification" $
      VirtualBoxSpecification
        <$> optionalFieldWithDefault "running" True "whether the vm should be running" .= virtualBoxSpecificationRunning
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
    VirtualBoxOutput
virtualBoxProvider =
  Provider
    { providerName = "virtualbox",
      providerQuery = queryVirtualBox,
      providerApply = applyVirtualBox,
      providerCheck = checkVirtualBox,
      providerDestroy = destroyVirtualBox
    }

queryVirtualBox :: ResourceName -> P (QueryResult VirtualBoxOutput)
queryVirtualBox resourceName = do
  mTups <- getVMInfo resourceName
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
  P (ApplyResult VirtualBoxOutput)
applyVirtualBox resourceName VirtualBoxSpecification {..} = do
  mVMInfo <- getVMInfo resourceName

  -- Step 1. Make sure the VM exists
  VirtualBoxInfo {..} <- case mVMInfo of
    Nothing -> do
      logDebugN "Creating a brand new VM."
      (_, _) <- makeVirtualBox resourceName virtualBoxSpecificationBaseFolder
      mVMInfoAfter <- getVMInfo resourceName
      case mVMInfoAfter of
        Nothing -> fail "VM Should have existed by now."
        Just vmInfo -> pure vmInfo
    Just vmInfo@VirtualBoxInfo {..} -> do
      if isProperPrefixOf virtualBoxSpecificationBaseFolder virtualBoxInfoSettingsFile
        then pure vmInfo
        else do
          logDebugN "VM is deployed a different settings file, recreating the virtual box."
          when (virtualBoxInfoVMState == VMStateRunning) $ turnOffVM resourceName
          unregisterVirtualBox resourceName
          (_, _) <- makeVirtualBox resourceName virtualBoxSpecificationBaseFolder
          mVMInfoAfter <- getVMInfo resourceName
          case mVMInfoAfter of
            Nothing -> fail "VM Should have existed by now."
            Just vmInfo -> pure vmInfo

  -- Step 2. Make sure that the VM is in the right state (turned on)
  case (virtualBoxInfoVMState, virtualBoxSpecificationRunning) of
    (VMStatePoweroff, False) -> logDebugN "VM is turned off, leaving it as-is."
    (VMStateRunning, True) -> logDebugN "VM is already running, leaving it as-is."
    (VMStateRunning, False) -> do
      logDebugN "VM is deployed, and turned on, turning it off."
      turnOffVM resourceName
    (VMStatePoweroff, True) -> do
      logDebugN "VM is not running yet, turning it on."
      turnOnVM resourceName

  let output =
        VirtualBoxOutput
          { virtualBoxOutputUUID = virtualBoxInfoUUID,
            virtualBoxOutputSettingsFile = virtualBoxInfoSettingsFile
          }
  pure $ ApplySuccess output

checkVirtualBox :: ResourceName -> VirtualBoxSpecification -> P (CheckResult VirtualBoxOutput)
checkVirtualBox resourceName VirtualBoxSpecification {..} = do
  mTups <- getVMInfo resourceName
  case mTups of
    Nothing -> fail $ unwords ["VM does not exist:", show resourceName]
    Just VirtualBoxInfo {..} -> do
      if isProperPrefixOf virtualBoxSpecificationBaseFolder virtualBoxInfoSettingsFile
        then do
          let stateCorrect = case virtualBoxInfoVMState of
                VMStateRunning -> virtualBoxSpecificationRunning
                VMStatePoweroff -> not virtualBoxSpecificationRunning
          if stateCorrect
            then
              pure $
                CheckSuccess $
                  VirtualBoxOutput
                    { virtualBoxOutputUUID = virtualBoxInfoUUID,
                      virtualBoxOutputSettingsFile = virtualBoxInfoSettingsFile
                    }
            else
              fail $
                unlines
                  [ "VM exists but is not in the correct state:",
                    unwords ["State:", show virtualBoxInfoVMState],
                    unwords ["Should be running:", show virtualBoxSpecificationRunning]
                  ]
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

destroyVirtualBox :: ResourceName -> P DestroyResult
destroyVirtualBox resourceName = do
  mVmInfo <- getVMInfo resourceName
  case mVmInfo of
    Nothing -> pure DestroySuccess
    Just VirtualBoxInfo {..} -> do
      when (virtualBoxInfoVMState == VMStateRunning) (turnOffVM resourceName)
      unregisterVirtualBox resourceName
      pure DestroySuccess

unregisterVirtualBox :: ResourceName -> P ()
unregisterVirtualBox resourceName = do
  logDebugN $ T.pack $ unwords ["Unregistering virtualbox", show resourceName]
  pc <-
    liftIO $
      mkVBoxManageProcessConfig
        [ "unregistervm",
          resourceNameString resourceName,
          "--delete"
        ]
  logProcessConfig pc
  ec <- runProcess pc
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> fail $ unwords ["VBoxManage unregister failed with exit code:", show c]

getVMInfo :: ResourceName -> P (Maybe VirtualBoxInfo)
getVMInfo resourceName = do
  pc <-
    liftIO $
      mkVBoxManageProcessConfig
        [ "showvminfo",
          resourceNameString resourceName,
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
              [key, val] -> do
                v <- readMaybe (T.unpack val)
                pure (key, v)
              _ -> Nothing
      logDebugN $ T.pack $ unlines ("Parsed tuples:" : map show (sort tups))
      let requireVal key = case lookup key tups of
            Nothing ->
              throwP $
                ProviderException $
                  unlines $
                    unwords ["Expected to have found this key in the vminfo output:", show key] :
                    map show tups
            Just val -> pure val
      uuidVal <- requireVal "UUID"
      virtualBoxInfoUUID <- case UUID.fromString $ T.unpack uuidVal of
        Nothing -> fail $ unwords ["uuid not readable:", show uuidVal]
        Just u -> pure u
      settingsFileVal <- requireVal "CfgFile"
      virtualBoxInfoSettingsFile <- case parseAbsFile $ T.unpack settingsFileVal of
        Nothing -> fail $ unwords ["settings file not readable:", show settingsFileVal]
        Just settingsFile -> pure settingsFile
      stateVal <- requireVal "VMState"
      virtualBoxInfoVMState <- case stateVal of
        "running" -> pure VMStateRunning
        "poweroff" -> pure VMStatePoweroff
        _ -> fail $ unwords ["Unknown vm state: ", show stateVal]
      pure $ Just $ VirtualBoxInfo {..}
    ExitFailure _ -> pure Nothing

data VirtualBoxInfo = VirtualBoxInfo
  { virtualBoxInfoUUID :: !UUID,
    virtualBoxInfoSettingsFile :: !(Path Abs File),
    virtualBoxInfoVMState :: !VMState
  }
  deriving (Show, Eq, Generic)

data VMState
  = VMStateRunning
  | VMStatePoweroff
  deriving (Show, Eq, Generic)

makeVirtualBox :: ResourceName -> Path Abs Dir -> P (UUID, Path Abs File)
makeVirtualBox resourceName baseFolder = do
  logDebugN "Creating the VM settings file."
  let args =
        [ "createvm",
          "--name",
          T.unpack $ resourceNameText resourceName,
          "--ostype",
          "Linux_64",
          "--basefolder",
          fromAbsDir baseFolder,
          "--register"
        ]
  pc <- liftIO $ mkVBoxManageProcessConfig args
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

turnOnVM :: ResourceName -> P ()
turnOnVM resourceName = do
  logDebugN "Turning on VM"
  let args =
        [ "startvm",
          T.unpack $ resourceNameText resourceName,
          "--type",
          "headless"
        ]
  pc <- liftIO $ mkVBoxManageProcessConfig args
  logProcessConfig pc
  ec <- runProcess pc
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> fail $ unwords ["startvm failed with exit code:", show c]

turnOffVM :: ResourceName -> P ()
turnOffVM resourceName = do
  logDebugN "Turning off VM"
  let args =
        [ "controlvm",
          T.unpack $ resourceNameText resourceName,
          "poweroff"
        ]
  pc <- liftIO $ mkVBoxManageProcessConfig args
  logProcessConfig pc
  ec <- runProcess pc
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> fail $ unwords ["startvm failed with exit code:", show c]

mkVBoxManageProcessConfig :: [String] -> IO (ProcessConfig () () ())
mkVBoxManageProcessConfig args = do
  env <- System.getEnvironment
  let allowedVars = ["HOME", "LANG", "LOCALE_ARCHIVE"]
  let pc = setEnv (filter ((`elem` allowedVars) . fst) env) $ proc "VBoxManage" args
  pure pc

logProcessConfig :: ProcessConfig input output error -> P ()
logProcessConfig pc = logDebugN $ T.pack $ show pc
