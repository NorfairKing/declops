{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Declops.Provider.ResourceName where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist.Sqlite
import GHC.Generics (Generic)

newtype ResourceName = ResourceName {unResourceName :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, IsString, FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec ResourceName)

instance Validity ResourceName

instance HasCodec ResourceName where
  codec = dimapCodec ResourceName unResourceName codec

instance PersistField ResourceName where
  toPersistValue = toPersistValue . unResourceName
  fromPersistValue = fmap ResourceName . fromPersistValue

instance PersistFieldSql ResourceName where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
