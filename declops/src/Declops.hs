{-# LANGUAGE OverloadedStrings #-}

module Declops where

import Data.List
import Data.Validity
import Data.Validity.Path ()
import Declops.Command
import Declops.OptParse
import Declops.Provider
import GHC.Generics (Generic)
import Path
import Path.IO

declopsMain :: IO ()
declopsMain = do
  Instructions dispatch settings <- getInstructions
  print dispatch
  case dispatch of
    DispatchApply applySettings -> declopsApply applySettings
