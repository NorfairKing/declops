{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.Local.TempFileSpec (spec) where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.List
import Declops.Provider.Local.TempFile
import Declops.Provider.Test
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

instance GenValid TempFileSpecification

instance GenValid TempFileOutput

spec :: Spec
spec = do
  providerJSONSpec tempFileProvider
  modifyMaxSuccess (`div` 10) $
    sequential $
      before_ cleanupTempFiles $
        after_ cleanupTempFiles $
          localProviderSpec
            False
            tempFileProvider
            (\_ -> pure ())
            (\_ -> TempFileSpecification <$> genValid)

cleanupTempFiles :: IO ()
cleanupTempFiles = do
  tmpDir <- resolveDir' "/tmp"
  files <- snd <$> listDir tmpDir
  let declopsFiles = filter (("/tmp/declops-" `isPrefixOf`) . fromAbsFile) files
  mapM_ removeFile declopsFiles
