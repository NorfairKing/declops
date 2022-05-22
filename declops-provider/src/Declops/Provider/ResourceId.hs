{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Provider.ResourceId where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Data.Aeson as JSON
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist.Sqlite
import Declops.Provider.ProviderName
import Declops.Provider.ResourceName
import GHC.Generics (Generic)

data ResourceId = ResourceId
  { resourceIdProvider :: !ProviderName,
    resourceIdResource :: !ResourceName
  }
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ResourceId)

instance Validity ResourceId

instance Show ResourceId where
  show = show . renderResourceId

instance IsString ResourceId where
  fromString s = case parseResourceId (fromString s) of
    Left err -> error err
    Right rid -> rid

instance HasCodec ResourceId where
  codec = bimapCodec parseResourceId renderResourceId codec

instance PersistField ResourceId where
  toPersistValue = toPersistValue . renderResourceId
  fromPersistValue = fromPersistValue >=> left T.pack . parseResourceId

instance PersistFieldSql ResourceId where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

parseResourceId :: Text -> Either String ResourceId
parseResourceId t = case T.splitOn "." t of
  [providerText, resourceText] -> Right $ ResourceId (ProviderName providerText) (ResourceName resourceText)
  _ -> Left "Unparseable resource id"

renderResourceId :: ResourceId -> Text
renderResourceId ResourceId {..} =
  T.intercalate
    "."
    [ unProviderName resourceIdProvider,
      unResourceName resourceIdResource
    ]
