{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Declops.DB.ProviderName where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Proxy
import Data.String
import Data.Text (Text)
import Database.Persist.Sqlite
import GHC.Generics (Generic)

newtype ProviderName = ProviderName {unProviderName :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, IsString, FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec ProviderName)

instance HasCodec ProviderName where
  codec = dimapCodec ProviderName unProviderName codec

instance PersistField ProviderName where
  toPersistValue = toPersistValue . unProviderName
  fromPersistValue = fmap ProviderName . fromPersistValue

instance PersistFieldSql ProviderName where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
