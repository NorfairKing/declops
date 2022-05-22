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
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
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

instance Validity VirtualBoxSpecification

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
      providerQuery = \reference -> do
        ec <- runProcess $ proc "VBoxManage" ["showvminfo", UUID.toString reference, "--details", "--machinereadable"]
        -- This isn't entirely right, but it's a start
        pure $ case ec of
          ExitSuccess -> ExistsRemotely VirtualBoxOutput
          ExitFailure _ -> DoesNotExistRemotely,
      providerApply = \VirtualBoxSpecification {..} applyContext -> undefined,
      providerCheck = \VirtualBoxSpecification {..} reference -> undefined,
      providerDestroy = \reference -> undefined
    }
