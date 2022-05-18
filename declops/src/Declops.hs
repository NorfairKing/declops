module Declops where

import Declops.Command
import Declops.OptParse

declopsMain :: IO ()
declopsMain = do
  Instructions dispatch settings <- getInstructions
  case dispatch of
    DispatchQuery querySettings -> declopsQuery querySettings settings
    DispatchApply applySettings -> declopsApply applySettings settings
