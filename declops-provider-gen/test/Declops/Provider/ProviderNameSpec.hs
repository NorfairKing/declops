{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Declops.Provider.ProviderNameSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.Provider.Gen ()
import Declops.Provider.ProviderName
import Test.Syd
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  persistSpec @ProviderName
  jsonSpec @ProviderName
