{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.TempFileSpec (spec) where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.Provider.TempFile
import Declops.Provider.Test
import Path
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

instance GenValid TempFileSpecification

instance GenValid TempFileOutput

spec :: Spec
spec = do
  providerJSONSpec tempFileProvider
  tempDirSpec "declops-temporary-file-provider-test" $
    localProviderSpec
      tempFileProvider
      (\tdir -> (</>) tdir <$> genValid)
      (\tdir -> TempFileSpecification tdir <$> elements ["foo", "bar", "quux"] <*> genValid)
