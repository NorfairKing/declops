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
            [ ("temporary-file", [("foo", ["temporary-directory.bar"])]),
              ("temporary-directory", [("bar", [])])
            ]
       in case parseDependenciesSpecification graph of
            Left err -> expectationFailure $ show err
            Right actual -> removeProviders actual `shouldBe` graph

    it "detects that there is a missing resource in this graph" $ do
      let graph =
            [ ("temporary-file", [("foo", ["temporary-file.bar"])])
            ]
      case parseDependenciesSpecification graph of
        Right _ -> expectationFailure "should not have succeeded"
        Left err -> err `shouldBe` DependenciesSpecificationMissingResources ["temporary-file.bar"]

    it "detects that there is a missing provider in this graph" $ do
      let graph =
            [ ("unknown-provider", [("foo", [])])
            ]
      case parseDependenciesSpecification graph of
        Right _ -> expectationFailure "should not have succeeded"
        Left err -> err `shouldBe` DependenciesSpecificationUnknownProvider ["unknown-provider"]

    it "produces valid dependencies specifications" $
      forAllValid $ \m ->
        isValid (parseDependenciesSpecification m)

  describe "nixEvalGraph" $
    it "detects the right dependencies in this real-world example" $ do
      dependenciesSpecification <- testC "simple-success.nix" nixEvalGraph
      removeProviders dependenciesSpecification
        `shouldBe` [ ( "temporary-directory",
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
