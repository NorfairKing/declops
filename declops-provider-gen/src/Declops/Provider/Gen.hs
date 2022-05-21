{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Declops.Provider.ProviderName
import Declops.Provider.ResourceName

instance GenValid ProviderName

instance GenValid ResourceName
