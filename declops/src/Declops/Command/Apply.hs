module Declops.Command.Apply (declopsApply) where

import Declops.OptParse

declopsApply :: ApplySettings -> IO ()
declopsApply = print
