{-# LANGUAGE OverloadedStrings #-}

module Declops.Command.Destroy (declopsDestroy) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Declops.DB
import Declops.Env
import Declops.Provider
import UnliftIO

declopsDestroy :: C ()
declopsDestroy = do
  logDebugN "Parsing specification"
  specifications <- nixEval

  tups <- forConcurrently specifications $
    \(SomeSpecification resourceTypeName currentResourceName specification provider) -> do
      logDebugN $
        T.pack $
          unwords
            [ "Destroying current state of",
              concat [T.unpack resourceTypeName, ".", T.unpack currentResourceName]
            ]
      mLocalResource <- runDB $ getBy $ UniqueResource resourceTypeName currentResourceName
      destroyResult <- undefined :: C DestroyResult
      pure (currentResourceName, destroyResult)
  liftIO $ mapM_ print tups
