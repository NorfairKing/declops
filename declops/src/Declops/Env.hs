{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql
import Database.Persist.Sqlite
import Declops.DB
import Declops.OptParse (Settings (..))
import Declops.Provider
import Declops.Provider.Local.TempDir
import Declops.Provider.Local.TempFile
import Declops.Provider.ResourceId
import Declops.Provider.VirtualBox
import Path
import System.IO (stderr)
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout

type C a = ReaderT Env (LoggingT IO) a

data Env = Env
  { envDeploymentFile :: !(Path Abs File),
    envConnectionPool :: !ConnectionPool,
    envTerminalCapabilities :: !TerminalCapabilities
  }

runDB :: SqlPersistT IO a -> C a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool

runC :: Settings -> C () -> IO ()
runC Settings {..} func = do
  envTerminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  runColouredStderrLoggingT envTerminalCapabilities $
    filterLogger (\_ ll -> ll >= settingLogLevel) $
      withSqlitePool (T.pack (fromAbsFile settingStateFile)) 1 $ \pool -> do
        runSqlPool (runMigration localMigration) pool
        let envDeploymentFile = settingDeploymentFile
        let envConnectionPool = pool
        runReaderT func Env {..}

allProviders :: Map ProviderName JSONProvider
allProviders =
  let p ::
        ( FromJSON specification,
          FromJSON reference,
          ToJSON reference,
          FromJSON output,
          ToJSON output
        ) =>
        Provider specification reference output ->
        (ProviderName, JSONProvider)

      p provider = (providerName provider, toJSONProvider provider)
   in M.fromList
        [ p tempDirProvider,
          p tempFileProvider,
          p virtualBoxProvider
        ]

showJSON :: JSON.Value -> String
showJSON = T.unpack . TE.decodeUtf8 . LB.toStrict . JSON.encodePretty

putChunks :: [Chunk] -> C ()
putChunks cs = do
  terminalCapablities <- asks envTerminalCapabilities
  liftIO $ putChunksWith terminalCapablities cs

putTable :: [[Chunk]] -> C ()
putTable = putChunks . layoutAsTable

providerNameChunk :: ProviderName -> Chunk
providerNameChunk = fore yellow . chunk . unProviderName

resourceNameChunk :: ResourceName -> Chunk
resourceNameChunk = fore yellow . chunk . unResourceName

resourceIdChunk :: ResourceId -> Chunk
resourceIdChunk = fore yellow . chunk . renderResourceId

withResourceIdSource :: ResourceId -> C a -> C a
withResourceIdSource resourceId (ReaderT func) = ReaderT $ \env ->
  renameSource (renderResourceId resourceId) (func env)

renameSource :: Text -> LoggingT m a -> LoggingT m a
renameSource looperName = modLogSource $ \source -> if source == "" then looperName else source

modLogSource :: (LogSource -> LogSource) -> LoggingT m a -> LoggingT m a
modLogSource func (LoggingT mFunc) = LoggingT $ \logFunc ->
  let newLogFunc loc source level str =
        let source' = func source
         in logFunc loc source' level str
   in mFunc newLogFunc

runColouredStderrLoggingT :: TerminalCapabilities -> LoggingT m a -> m a
runColouredStderrLoggingT caps (LoggingT func) = func logFunc
  where
    logFunc loc source level str = do
      let addColour = case level of
            LevelDebug -> id
            LevelInfo -> fore white
            LevelWarn -> fore yellow
            LevelError -> fore red
      let levelChunk = addColour $ case level of
            LevelDebug -> "DEBUG"
            LevelInfo -> "INFO"
            LevelWarn -> "WARNING"
            LevelError -> "ERROR"
          sourceChunk = addColour $ chunk source
          logStrChunk = addColour $ chunk $ TE.decodeUtf8 $ fromLogStr str
      let chunks = [levelChunk, " ", sourceChunk, " ", logStrChunk, "\n"]
      hPutChunksWith caps stderr chunks
