{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Provider.ResourceId where

import Autodocodec
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import Declops.Provider.ProviderName
import Declops.Provider.ResourceName
import GHC.Generics (Generic)

data ResourceId = ResourceId
  { resourceIdProvider :: !ProviderName,
    resourceIdResource :: !ResourceName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ResourceId)

instance HasCodec ResourceId where
  codec = bimapCodec parseResourceId renderResourceId codec

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
