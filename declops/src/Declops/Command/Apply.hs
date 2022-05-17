{-# LANGUAGE QuasiQuotes #-}

module Declops.Command.Apply (declopsApply) where

import Declops.OptParse
import Declops.Provider
import Declops.Provider.TempDir
import Path
import System.Exit

declopsApply :: ApplySettings -> IO ()
declopsApply _ = do
  applyResult <- providerApply tempDirProvider sampleConfig DoesNotExistLocallyNorRemotely
  case applyResult of
    ApplyFailure err -> die err
    ApplySuccess reference output -> print (reference, output)

sampleConfig :: TempDirSpecification
sampleConfig =
  TempDirSpecification
    { tempDirSpecificationBase = [absdir|/tmp|],
      tempDirSpecificationTemplate = "foobar"
    }
