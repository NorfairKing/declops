{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBoxSpec (spec) where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.Provider.Test
import Declops.Provider.VirtualBox
import Path
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

instance GenValid VirtualBoxSpecification

instance GenValid VirtualBoxOutput

spec :: Spec
spec = do
  providerJSONSpec virtualBoxProvider

-- tempDirSpec "declops-temporary-virtualbox-provider-test" $
--   localProviderSpec
--     virtualBoxProvider
--     (\tdir -> (</>) tdir <$> genValid)
--     (\tdir -> TempDirSpecification tdir <$> elements ["foo", "bar", "quux"])
