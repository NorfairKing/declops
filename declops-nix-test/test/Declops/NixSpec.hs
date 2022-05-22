{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Declops.NixSpec (spec) where

import Data.GenValidity.Containers ()
import Declops.Command.TestUtils
import Declops.Nix
import Declops.Provider.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseDependenciesSpecification" $ do
    it "detects no issues in this example" $
      let graph =
            [ ("foo", [("bar", ["quux.mu"])]),
              ("quux", [("mu", [])])
            ]
       in parseDependenciesSpecification graph
            `shouldBe` Right (DependenciesSpecification graph)

    it "detects that there is a missing resource in this graph" $ do
      let graph =
            [ ("foo", [("bar", ["quux.mu"])])
            ]
      case parseDependenciesSpecification graph of
        Right _ -> expectationFailure "should not have succeeded"
        Left err -> err `shouldBe` DependenciesSpecificationMissingResources ["quux.mu"]

    it "produces valid dependencies specifications" $
      producesValid parseDependenciesSpecification

  describe "nixEvalGraph" $
    it "detects the right dependencies in this real-world example" $
      testC "simple-success.nix" nixEvalGraph
        `shouldReturn` DependenciesSpecification
          [ ( "temporary-directory",
              [ ("my-other-temp-dir", ["temporary-directory.my-temp-dir"]),
                ("my-temp-dir", [])
              ]
            ),
            ( "temporary-file",
              [ ( "my-other-temp-file",
                  ["temporary-directory.my-other-temp-dir"]
                ),
                ("my-temp-file", ["temporary-directory.my-temp-dir"])
              ]
            )
          ]
