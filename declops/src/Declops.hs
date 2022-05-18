module Declops where

import Declops.Command
import Declops.Env
import Declops.OptParse

declopsMain :: IO ()
declopsMain = do
  Instructions dispatch settings <- getInstructions
  runC settings $ case dispatch of
    DispatchQuery querySettings -> declopsQuery querySettings
    DispatchApply applySettings -> declopsApply applySettings
