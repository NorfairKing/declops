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

data VirtualBoxSpecification = VirtualBoxSpecification
  { virtualBoxSpecificationName :: !Text,
    virtualBoxSpecificationBaseFolder :: !Text -- Text because of json roundtrips
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
    virtualBoxOutputSettingsFile :: !Text -- Because of the json roundtrip
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

queryVirtualBox :: UUID -> P (RemoteState VirtualBoxOutput)
queryVirtualBox uuid = liftIO $ do
  (ec, output) <-
    readProcessStdout $
      proc
        "VBoxManage"
        [ "showvminfo",
          UUID.toString uuid,
          "--details",
          "--machinereadable"
        ]

  case ec of
    ExitSuccess -> do
      let tups = map (T.breakOn "=") (T.lines (TE.decodeUtf8 (LB.toStrict output)))
      virtualBoxOutputUUID <- case lookup "UUID" tups of
        Nothing -> die "uuid not found." -- TODO
        Just uuidText -> case UUID.fromText uuidText of
          Nothing -> die "uuid not readable"
          Just uuid -> pure uuid
      virtualBoxOutputSettingsFile <- case lookup "CfgFile" tups of
        Nothing -> die "settings file not found."
        Just stf -> pure stf
      pure $ ExistsRemotely VirtualBoxOutput {..}
    ExitFailure _ -> pure DoesNotExistRemotely

applyVirtualBox ::
  VirtualBoxSpecification ->
  ApplyContext UUID VirtualBoxOutput ->
  P (ApplyResult UUID VirtualBoxOutput)
applyVirtualBox VirtualBoxSpecification {..} applyContext =
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      logDebugN "Creating a brand new VM."
      (uuid, settingsFile) <- makeVirtualBox virtualBoxSpecificationName virtualBoxSpecificationBaseFolder Nothing
      registerVirtualBox settingsFile
      let output = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = T.pack $ fromAbsFile settingsFile}
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
      let output = VirtualBoxOutput {virtualBoxOutputUUID = uuid, virtualBoxOutputSettingsFile = T.pack $ fromAbsFile settingsFile}
      pure $ ApplySuccess uuid output

checkVirtualBox :: VirtualBoxSpecification -> UUID -> P (CheckResult VirtualBoxOutput)
checkVirtualBox VirtualBoxSpecification {..} uuid = undefined

destroyVirtualBox :: UUID -> P DestroyResult
destroyVirtualBox uuid = undefined

makeVirtualBox :: Text -> Text -> Maybe UUID -> P (UUID, Path Abs File)
makeVirtualBox name baseFolder mUuid = do
  logDebugN "Creating the VM settings file."
  (ec, output) <-
    readProcessStdout $
      proc
        "VBoxManage"
        [ "createvm",
          "--name",
          T.unpack name,
          "--ostype",
          "Linux_64",
          "--basefolder",
          T.unpack baseFolder
        ]
  case ec of
    ExitFailure c -> throwP $ ApplyException $ unwords ["createvm failed with exit code:", show c]
    ExitSuccess -> do
      let tups = flip mapMaybe (SB8.lines (LB.toStrict output)) $ \t ->
            case T.splitOn ": " $ TE.decodeUtf8With TE.lenientDecode t of
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
      settingsFileVal <- requireVal "Settings file"
      settingsFile <- resolveFile' $ T.unpack settingsFileVal
      pure (uuid, settingsFile)

predictSettionsFile :: Text -> Text -> P (Path Abs File)
predictSettionsFile name baseFolder = do
  baseDir <- resolveDir' $ T.unpack baseFolder
  settingsFile <-
    resolveFile baseDir $
      let n = T.unpack name
       in concat [n, "/", n, ".vbox"]
  logDebugN $ T.pack $ unwords ["Predicting that the settings file will be at", fromAbsFile settingsFile]
  pure settingsFile

registerVirtualBox :: Path Abs File -> P ()
registerVirtualBox settingsFile = do
  logDebugN "Registering the VM."
  ec <- runProcess $ proc "VBoxManage" ["registervm", fromAbsFile settingsFile]
  case ec of
    ExitFailure c -> throwP $ ApplyException $ unwords ["registervm failed with exit code:", show c]
    ExitSuccess -> pure ()
