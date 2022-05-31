{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Declops.Provider.ResourceNameSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.Provider.Gen ()
import Declops.Provider.ResourceName
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  persistSpec @ResourceName
  jsonSpec @ResourceName
  describe "Validity ResourceName" $ do
    it "considers these examples valid" $ do
      shouldBeValid $ ResourceName "foo"
      shouldBeValid $ ResourceName "foo_bar"
      shouldBeValid $ ResourceName "foo-bar"
      shouldBeInvalid $ ResourceName "foo bar"
      shouldBeInvalid $ ResourceName "foo\nbar"
      shouldBeInvalid $ ResourceName ""
