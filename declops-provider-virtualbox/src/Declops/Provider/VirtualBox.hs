{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBox where

import Autodocodec
import Control.Arrow (left)
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
        <*> requiredField "basefolder" "base folder" .= virtualBoxSpecificationBaseFolder

data VirtualBoxOutput = VirtualBoxOutput
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxOutput)

instance Validity VirtualBoxOutput

instance HasCodec VirtualBoxOutput where
  codec =
    object "VirtualBoxOutput" $
      pure VirtualBoxOutput

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

queryVirtualBox :: UUID -> IO (RemoteState VirtualBoxOutput)
queryVirtualBox uuid = do
  ec <-
    runProcess $
      proc
        "VBoxManage"
        [ "showvminfo",
          UUID.toString uuid,
          "--details",
          "--machinereadable"
        ]
  -- This isn't entirely right, but it's a start
  pure $ case ec of
    ExitSuccess -> ExistsRemotely VirtualBoxOutput
    ExitFailure _ -> DoesNotExistRemotely

applyVirtualBox ::
  VirtualBoxSpecification ->
  ApplyContext UUID VirtualBoxOutput ->
  IO (ApplyResult UUID VirtualBoxOutput)
applyVirtualBox VirtualBoxSpecification {..} applyContext =
  case applyContext of
    DoesNotExistLocallyNorRemotely -> do
      (ec, output) <-
        readProcessStdout $
          proc
            "VBoxManage"
            [ "createvm",
              "--name",
              T.unpack virtualBoxSpecificationName,
              "--ostype",
              "Linux_64",
              "--basefolder",
              T.unpack virtualBoxSpecificationBaseFolder
            ]
      case ec of
        ExitFailure _ -> pure $ ApplyFailure "Failed to create the vm."
        ExitSuccess -> do
          let tups = flip mapMaybe (SB8.lines (LB.toStrict output)) $ \t ->
                case T.splitOn ": " $ TE.decodeUtf8With TE.lenientDecode t of
                  [name, val] -> Just (name, val)
                  _ -> Nothing
          case lookup "UUID" tups >>= UUID.fromText of
            Nothing -> pure $ ApplyFailure "Expected to have found a uuid."
            Just uuid ->
              pure $ ApplySuccess uuid VirtualBoxOutput

checkVirtualBox :: VirtualBoxSpecification -> UUID -> IO (CheckResult VirtualBoxOutput)
checkVirtualBox = undefined

destroyVirtualBox :: UUID -> IO DestroyResult
destroyVirtualBox = undefined
