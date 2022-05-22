{-# LANGUAGE LambdaCase #-}

module Declops.CommandSpec (spec) where

import Declops.Command
import Declops.Command.TestUtils
import System.Exit
import Test.Syd
import UnliftIO

spec :: Spec
spec = sequential $ do
  it "Can go through an entire cycle for this example deployment" $ do
    testC "simple-success.nix" $ do
      declopsQuery
      (declopsCheck >> throwIO ExitSuccess)
        `catch` ( \case
                    ExitSuccess -> liftIO $ expectationFailure "check 1 should not have succeeded."
                    ExitFailure _ -> pure () -- Fails
                )
      declopsDestroy
      declopsApply
      declopsApply
      declopsQuery
      declopsCheck
      declopsDestroy
      declopsQuery
      (declopsCheck >> throwIO ExitSuccess)
        `catch` ( \case
                    ExitSuccess -> liftIO $ expectationFailure "check 3 should not have succeeded."
                    ExitFailure _ -> pure () -- Fails
                )
