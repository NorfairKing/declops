module Declops where

import Declops.Command
import Declops.OptParse

declopsMain :: IO ()
declopsMain = do
  Instructions dispatch settings <- getInstructions
  print dispatch
  case dispatch of
    DispatchApply ApplySettings -> declopsApply settings
