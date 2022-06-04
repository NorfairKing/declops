{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBox.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.UUID ()
import Declops.Provider.VirtualBox

instance GenValid VirtualBoxSpecification

instance GenValid VirtualBoxOutput
