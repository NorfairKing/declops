{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Declops.NixSpec (spec) where

import Declops.Command.TestUtils
import Declops.Nix
import Test.Syd

spec :: Spec
spec = do
  it "detects the right dependencies in this example" $
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
