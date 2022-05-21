{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.DBSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.DB
import Declops.Provider.Gen ()
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity
import Test.Syd.Validity.Persist

instance GenValid ResourceReference

spec :: Spec
spec = do
  persistSpec @ResourceReference
  sqliteMigrationSucceedsSpec "test_resources/db/migration.sql" localMigration
