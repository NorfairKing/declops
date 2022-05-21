{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.ResourceNameSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.Provider.ResourceName
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

instance GenValid ResourceName

spec :: Spec
spec = do
  persistSpec @ResourceName
  jsonSpec @ResourceName
