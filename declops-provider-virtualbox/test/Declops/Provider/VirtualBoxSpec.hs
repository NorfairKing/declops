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
import System.Exit
import System.Process.Typed
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
        before_ cleanupVMs $
          afterAll_ cleanupVMs $
            tempDirSpec tempDirTemplate $
              localProviderSpec
                False
                virtualBoxProvider
                (\_ -> genValid)
                ( \tdir -> (`suchThat` isValid) $ do
                    virtualBoxSpecificationBaseFolder <- (</>) tdir <$> genValid
                    pure VirtualBoxSpecification {..}
                )

cleanupVMs :: IO ()
cleanupVMs = do
  let listPc =
        proc
          "VBoxManage"
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
        let unregisterPc =
              proc
                "VBoxManage"
                [ "unregistervm",
                  UUID.toString uuid
                ]
        unregisterEc <- runProcess unregisterPc
        case unregisterEc of
          ExitFailure c -> fail $ unwords ["VBoxManage unregister failed with exit code:", show c]
          ExitSuccess -> pure ()

tempDirTemplate :: FilePath
tempDirTemplate = "declops-temporary-virtualbox-provider-test"
