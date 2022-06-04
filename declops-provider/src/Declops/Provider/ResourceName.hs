{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Declops.Provider.ResourceName where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Char as Char
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text
import Database.Persist.Sqlite
import GHC.Generics (Generic)

newtype ResourceName = ResourceName {unResourceName :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord)
  deriving (FromJSON, ToJSON) via (Autodocodec ResourceName)

instance Validity ResourceName where
  validate rn@(ResourceName t) =
    mconcat
      [ genericValidate rn,
        declare "The resource name is not empty" $ not $ T.null t,
        declare "The resource name is not \"-\"" $ t /= "-",
        decorateText t validateResourceNameChar
      ]

validateResourceNameChar :: Char -> Validation
validateResourceNameChar = \case
  '-' -> valid
  '_' -> valid
  c ->
    mconcat
      [ declare "The character is printable" $ Char.isPrint c,
        declare "The character is in ASCII" $ Char.isAscii c,
        declare "The character is not a control char" $ not $ Char.isControl c,
        declare "The character is not a space" $ not $ Char.isSpace c,
        declare "The character is alpha numeric" $ Char.isAlphaNum c
      ]

resourceNameText :: ResourceName -> Text
resourceNameText = unResourceName

resourceNameString :: ResourceName -> String
resourceNameString = T.unpack . resourceNameText

parseResourceName :: Text -> Either String ResourceName
parseResourceName = prettyValidate . ResourceName

instance IsString ResourceName where
  fromString s = case parseResourceName (fromString s) of
    Left err -> error err
    Right rn -> rn

instance FromJSONKey ResourceName where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case parseResourceName t of
      Left err -> fail err
      Right r -> pure r

instance ToJSONKey ResourceName where
  toJSONKey = toJSONKeyText resourceNameText

instance HasCodec ResourceName where
  codec = bimapCodec parseResourceName unResourceName codec

instance PersistField ResourceName where
  toPersistValue = toPersistValue . resourceNameText
  fromPersistValue pv = do
    t <- fromPersistValue pv
    left T.pack $ parseResourceName t

instance PersistFieldSql ResourceName where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
