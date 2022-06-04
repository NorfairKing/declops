module Declops.Provider.VirtualBoxSpec (spec) where

import Declops.Provider.Test
import Declops.Provider.VirtualBox
import Declops.Provider.VirtualBox.Gen ()
import Test.Syd

spec :: Spec
spec = do
  providerJSONSpec virtualBoxProvider
