{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Declops.DB where

import Control.Arrow (left)
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "localMigration"]
  [persistLowerCase|

Resource
  name Text
  provider Text
  -- TODO do we need this?
  -- specification JSON.Value
  reference JSON.Value

  UniqueResource name provider

  deriving Show
  deriving Eq
  deriving Generic

|]

instance PersistField JSON.Value where
  toPersistValue = toPersistValue . LB.toStrict . JSON.encode
  fromPersistValue pv = do
    sb <- fromPersistValue pv
    left T.pack $ JSON.eitherDecode $ LB.fromStrict sb

instance PersistFieldSql JSON.Value where
  sqlType Proxy = SqlBlob
