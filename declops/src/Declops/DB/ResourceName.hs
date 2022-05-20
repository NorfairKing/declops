{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Declops.DB.ResourceName where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Proxy
import Data.Text (Text)
import Database.Persist.Sqlite
import GHC.Generics (Generic)

newtype ResourceName = ResourceName {unResourceName :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec ResourceName)

instance HasCodec ResourceName where
  codec = dimapCodec ResourceName unResourceName codec

instance PersistField ResourceName where
  toPersistValue = toPersistValue . unResourceName
  fromPersistValue = fmap ResourceName . fromPersistValue

instance PersistFieldSql ResourceName where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
