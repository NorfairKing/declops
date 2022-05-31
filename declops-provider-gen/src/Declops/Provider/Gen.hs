{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Gen where

import Data.Char
import Data.GenValidity
import Data.GenValidity.Text
import Declops.Provider.ProviderName
import Declops.Provider.ResourceId
import Declops.Provider.ResourceName
import Test.QuickCheck

instance GenValid ProviderName

instance GenValid ResourceId

instance GenValid ResourceName where
  genValid =
    ( ResourceName
        <$> genTextBy (choose (chr 0, chr 128) `suchThat` (validationIsValid . validateResourceNameChar))
    )
      `suchThat` isValid
