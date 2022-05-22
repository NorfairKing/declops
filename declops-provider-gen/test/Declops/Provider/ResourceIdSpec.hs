{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Declops.Provider.ResourceIdSpec (spec) where

import Data.GenValidity.Aeson ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Declops.Provider.Gen ()
import Declops.Provider.ResourceId
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  describe "renderResourceId" $
    it "renders any resourceId" $
      forAllValid $ \resourceId ->
        shouldBeValid $ renderResourceId resourceId

  describe "parseResourceId" $
    it "roundtrips with renderResourceId" $
      forAllValid $ \resourceId ->
        parseResourceId (renderResourceId resourceId) `shouldBe` Right resourceId

  persistSpec @ResourceId
  jsonSpec @ResourceId
