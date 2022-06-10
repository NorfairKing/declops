{-# LANGUAGE RecordWildCards #-}

module Declops.Command.TestUtils where

import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import Declops.Env
import Path.IO
import Paths_declops_nix_test
import Text.Colour

testC :: FilePath -> C a -> IO a
testC deploymentFile func = do
  envDeploymentFile <- getDataFileName ("deployments/" <> deploymentFile) >>= resolveFile'
  let logFunc loc source level str = do
        _ <- evaluate loc
        _ <- evaluate source
        _ <- evaluate level
        _ <- evaluate str
        pure () -- No logging, but still evaluating the arguments
  flip runLoggingT logFunc $ do
    let envTerminalCapabilities = WithoutColours
    runReaderT func Env {..}
