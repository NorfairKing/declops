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
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO
import System.IO (hClose)

data VirtualBoxSpecification = VirtualBoxSpecification
  { virtualBoxSpecificationName :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxSpecification)

instance Validity VirtualBoxSpecification

instance HasCodec VirtualBoxSpecification where
  codec =
    object "VirtualBoxSpecification" $
      VirtualBoxSpecification
        <$> requiredField "name" "name" .= virtualBoxSpecificationName

data VirtualBoxOutput = VirtualBoxOutput
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VirtualBoxOutput)

instance Validity VirtualBoxOutput

instance HasCodec VirtualBoxOutput where
  codec =
    object "VirtualBoxOutput" $
      pure VirtualBoxOutput

virtualBoxProvider :: Provider VirtualBoxSpecification (Path Abs File) VirtualBoxOutput
virtualBoxProvider =
  Provider
    { providerName = "virtualbox",
      providerQuery = \reference -> undefined,
      providerApply = \VirtualBoxSpecification {..} applyContext -> undefined,
      providerCheck = \VirtualBoxSpecification {..} reference -> undefined,
      providerDestroy = \reference -> undefined
    }
