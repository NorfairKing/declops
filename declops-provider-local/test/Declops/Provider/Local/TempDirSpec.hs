{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempDirSpec (spec) where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.List
import Declops.Provider.Local.TempDir
import Declops.Provider.Test
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

instance GenValid TempDirSpecification

instance GenValid TempDirOutput

spec :: Spec
spec = do
  providerJSONSpec tempDirProvider
  modifyMaxSuccess (`div` 10) $
    sequential $
      before_ cleanupTempDirs $
        after_ cleanupTempDirs $
          localProviderSpec
            False
            tempDirProvider
            (\_ -> pure ())
            (\_ -> pure TempDirSpecification)

cleanupTempDirs :: IO ()
cleanupTempDirs = do
  tmpDir <- resolveDir' "/tmp"
  dirs <- fst <$> listDir tmpDir
  let declopsDirs = filter (("/tmp/declops-" `isPrefixOf`) . fromAbsDir) dirs
  mapM_ removeDirRecur declopsDirs
