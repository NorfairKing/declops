{-# LANGUAGE OverloadedStrings #-}

module Declops.NixSpec (spec) where

import qualified Data.Map as M
import Declops.Command.TestUtils
import Declops.Env
import Declops.Nix
import Test.Syd

spec :: Spec
spec = do
  it "detects the right dependencies in this example" $
    testC "simple-success.nix" nixEvalGraph
      `shouldReturn` DependenciesSpecification
        ( M.fromList
            [ ( "temporary-directory",
                M.fromList
                  [ ( "my-other-temp-dir",
                      [ ResourceId
                          { resourceIdProvider = "temporary-directory",
                            resourceIdResource = "my-temp-dir"
                          }
                      ]
                    ),
                    ("my-temp-dir", [])
                  ]
              ),
              ( "temporary-file",
                M.fromList
                  [ ( "my-other-temp-file",
                      [ ResourceId
                          { resourceIdProvider = "temporary-directory",
                            resourceIdResource = "my-other-temp-dir"
                          }
                      ]
                    ),
                    ( "my-temp-file",
                      [ ResourceId
                          { resourceIdProvider = "temporary-directory",
                            resourceIdResource = "my-temp-dir"
                          }
                      ]
                    )
                  ]
              )
            ]
        )
