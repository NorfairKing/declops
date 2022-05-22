{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Declops.Env where

import Autodocodec
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
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
import GHC.Generics (Generic)
import Path
import Path.IO
import Paths_declops
import System.Exit
import System.IO (hClose)
import System.Process.Typed
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
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel) $
      withSqlitePool (T.pack (fromAbsFile settingStateFile)) 1 $ \pool -> do
        runSqlPool (runMigration localMigration) pool
        let envDeploymentFile = settingDeploymentFile
        let envConnectionPool = pool
        envTerminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
        runReaderT func Env {..}

nixEvalGraph :: C DependenciesSpecification
nixEvalGraph = do
  deploymentFile <- asks envDeploymentFile
  getGraphFile <- liftIO $ getDataFileName "nix-bits/get-graph.nix"
  nixEvalJSON
    [ "--file",
      getGraphFile,
      "dependencies",
      "--arg",
      "deploymentFile",
      fromAbsFile deploymentFile
    ]

newtype DependenciesSpecification = DependenciesSpecification
  { unDependenciesSpecification :: Map ProviderName (Map ResourceName [ResourceId])
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec DependenciesSpecification)

instance HasCodec DependenciesSpecification where
  codec = dimapCodec DependenciesSpecification unDependenciesSpecification codec

addProvidersToDependenciesSpecification :: DependenciesSpecification -> Either String (Map ProviderName (JSONProvider, Map ResourceName [ResourceId]))
addProvidersToDependenciesSpecification (DependenciesSpecification m) = fmap M.fromList $
  forM (M.toList m) $ \(providerName, resources) -> case M.lookup providerName allProviders of
    Nothing -> Left $ unwords ["Unknown provider:", T.unpack $ unProviderName providerName] -- TODO multiple errors
    Just provider -> Right (providerName, (provider, resources))

data ResourceId = ResourceId
  { resourceIdProvider :: !ProviderName,
    resourceIdResource :: !ResourceName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ResourceId)

instance HasCodec ResourceId where
  codec = bimapCodec parseResourceId renderResourceId codec

parseResourceId :: Text -> Either String ResourceId
parseResourceId t = case T.splitOn "." t of
  [providerText, resourceText] -> Right $ ResourceId (ProviderName providerText) (ResourceName resourceText)
  _ -> Left "Unparseable resource id"

renderResourceId :: ResourceId -> Text
renderResourceId ResourceId {..} =
  T.intercalate
    "."
    [ unProviderName resourceIdProvider,
      unResourceName resourceIdResource
    ]

nixEvalResourceSpecification :: Map ProviderName (Map ResourceName JSON.Value) -> ResourceId -> C JSON.Value
nixEvalResourceSpecification outputs ResourceId {..} = do
  deploymentFile <- asks envDeploymentFile
  -- withSystemTempFile "declops-resource-specification-eval" $ \outputsFile outputsFileHandle -> do
  tmpDir <- resolveDir' "/tmp"
  (outputsFile, outputsFileHandle) <- openTempFile tmpDir "declops-resource-specification-eval"
  do
    liftIO $ do
      LB.hPut outputsFileHandle $ JSON.encodePretty outputs
      hClose outputsFileHandle
    getSpecificationFile <- liftIO $ getDataFileName "nix-bits/get-specification.nix"
    nixEvalJSON
      [ "--file",
        getSpecificationFile,
        "output",
        "--arg",
        "deploymentFile",
        fromAbsFile deploymentFile,
        "--arg",
        "outputsFile",
        fromAbsFile outputsFile,
        "--argstr",
        "providerName",
        T.unpack (unProviderName resourceIdProvider),
        "--argstr",
        "resourceName",
        T.unpack (unResourceName resourceIdResource)
      ]

nixEvalJSON :: FromJSON a => [String] -> C a
nixEvalJSON args = do
  let allArgs = "eval" : "--json" : args

  logDebugN $
    T.pack $
      unwords
        [ "Running",
          show $ unwords $ "nix" : allArgs
        ]
  (exitCode, bs) <-
    liftIO $
      readProcessStdout $
        proc "nix" allArgs

  case exitCode of
    ExitFailure _ -> liftIO $ die "nix failed."
    ExitSuccess -> case JSON.eitherDecode bs of
      Left err -> liftIO $ die err
      Right output -> pure output

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
   in M.fromList [p tempDirProvider, p tempFileProvider]

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
