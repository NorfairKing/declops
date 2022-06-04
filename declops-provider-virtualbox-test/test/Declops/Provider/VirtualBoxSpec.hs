{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Declops.Provider.VirtualBoxSpec (spec) where

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.UUID ()
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.UUID as UUID
import Declops.Provider.Test
import Declops.Provider.VirtualBox
import Path
import Path.IO
import System.Environment as System
import System.Exit
import System.Process.Typed
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = do
  modifyMaxSuccess (const 1) $
    modifyMaxSize (const 10) $
      sequential $
        tempDirSpec tempDirTemplate $
          beforeWith withTmpHome $
            before_ cleanupVMs $
              localProviderSpec
                False
                virtualBoxProvider
                (\_ -> genValid)
                ( \tdir -> (`suchThat` isValid) $ do
                    virtualBoxSpecificationBaseFolder <- (</>) tdir <$> genValid
                    virtualBoxSpecificationRunning <- genValid
                    pure VirtualBoxSpecification {..}
                )

-- WARNING: Not threadsafe
--
-- We set `HOME` so that VBoxManage doesn't polute our homedir
withTmpHome :: Path Abs Dir -> IO (Path Abs Dir)
withTmpHome tdir = do
  tmpHomeDir <- resolveDir tdir "tmp-home"
  System.setEnv "HOME" (fromAbsDir tmpHomeDir)
  pure tdir

-- Cleanup all vms in the temporary directory before the test, because some
-- with the same will be made because we run property tests.
cleanupVMs :: IO ()
cleanupVMs = do
  listPc <-
    mkVBoxManageProcessConfig
      [ "list",
        "vms",
        "--long"
      ]
  (listEc, output) <- readProcessStdout listPc
  case listEc of
    ExitFailure c -> fail $ unwords ["VBoxManage list vms failed with exit code:", show c]
    ExitSuccess -> do
      let outputText = TE.decodeUtf8With TE.lenientDecode $ LB.toStrict output
      let tups = flip mapMaybe (T.lines outputText) $ \t ->
            case T.splitOn ":" t of
              [key, val] -> Just (key, T.strip val)
              _ -> Nothing
      let uuidVals = mapMaybe (\(k, v) -> if k == "UUID" then UUID.fromText v else Nothing) tups
      -- TODO only remove declops test vms?
      forM_ uuidVals $ \uuid -> do
        stopVmPC <-
          mkVBoxManageProcessConfig
            [ "controlvm",
              UUID.toString uuid,
              "poweroff"
            ]
        _ <- runProcess stopVmPC
        unregisterPc <-
          mkVBoxManageProcessConfig
            [ "unregistervm",
              UUID.toString uuid
            ]
        unregisterEc <- runProcess unregisterPc
        case unregisterEc of
          ExitFailure c -> fail $ unwords ["VBoxManage unregister failed with exit code:", show c]
          ExitSuccess -> pure ()

tempDirTemplate :: FilePath
tempDirTemplate = "declops-temporary-virtualbox-provider-test"
