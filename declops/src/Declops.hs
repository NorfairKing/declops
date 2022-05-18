module Declops where

import Declops.Command
import Declops.OptParse

declopsMain :: IO ()
declopsMain = do
  Instructions dispatch settings <- getInstructions
  case dispatch of
    DispatchApply applySettings -> declopsApply applySettings settings
