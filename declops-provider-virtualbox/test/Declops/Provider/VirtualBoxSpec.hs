{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBoxSpec (spec) where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.UUID ()
import qualified Data.Text as T
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
  modifyMaxSuccess (const 1) $
    modifyMaxSize (const 10) $
      sequential $
        tempDirSpec "declops-temporary-virtualbox-provider-test" $
          localProviderSpec
            virtualBoxProvider
            (\_ -> genValid)
            ( \tdir -> (`suchThat` isValid) $ do
                virtualBoxSpecificationName <- genValid `suchThat` (not . T.null)
                virtualBoxSpecificationBaseFolder <- T.pack . fromAbsDir <$> ((</>) tdir <$> genValid)

                pure VirtualBoxSpecification {..}
            )
