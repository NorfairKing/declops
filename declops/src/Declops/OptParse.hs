{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Declops.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

data Settings = Settings
  { settingStateFile :: !(Path Abs File),
    settingLogLevel :: !LogLevel
  }
  deriving (Show, Eq, Generic)

data Dispatch
  = DispatchQuery !QuerySettings
  | DispatchApply !ApplySettings
  deriving (Show, Eq, Generic)

data QuerySettings = QuerySettings
  { querySettingDeploymentFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

data ApplySettings = ApplySettings
  { applySettingDeploymentFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  settingStateFile <-
    resolveFile' $
      fromMaybe "declops-state.sqlite3" $
        flagStateFile <|> envStateFile <|> (mConf >>= configStateFile)
  let defaultDeploymentFile = "deployment.nix"
  dispatch <- case cmd of
    CommandQuery QueryArgs {..} -> do
      querySettingDeploymentFile <-
        resolveFile' $
          fromMaybe defaultDeploymentFile $
            queryArgDeploymentFile <|> envDeploymentFile <|> (mConf >>= configDeploymentFile)
      pure $ DispatchQuery QuerySettings {..}
    CommandApply ApplyArgs {..} -> do
      applySettingDeploymentFile <-
        resolveFile' $
          fromMaybe defaultDeploymentFile $
            applyArgDeploymentFile <|> envDeploymentFile <|> (mConf >>= configDeploymentFile)
      pure $ DispatchApply ApplySettings {..}
  let settingLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> (mConf >>= configLogLevel)
  pure $ Instructions dispatch Settings {..}

data Configuration = Configuration
  { configDeploymentFile :: !(Maybe FilePath),
    configStateFile :: !(Maybe FilePath),
    configLogLevel :: !(Maybe LogLevel)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "deployment-file" "deployment specification" .= configDeploymentFile
        <*> optionalField "state-file" "where to store deployment state" .= configStateFile
        <*> optionalFieldWith "log-level" (dimapCodec parseLogLevel renderLogLevel codec) "minimal severity of log messages" .= configLogLevel

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  here <- getCurrentDir
  resolveFile here "declops-config.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envDeploymentFile :: !(Maybe FilePath),
    envStateFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "DECLOPS_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.str) "DEPLOYMENT_FILE" (Env.def Nothing <> Env.help "Deployment file")
      <*> Env.var (fmap Just . Env.str) "STATE_FILE" (Env.def Nothing <> Env.help "State file")
      <*> Env.var (fmap (Just . parseLogLevel) . Env.str) "LOG_LEVEL" (Env.def Nothing <> Env.help "Minimal severity of log messages")

data Arguments
  = Arguments Command Flags
  deriving (Show, Eq, Generic)

getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

data Command
  = CommandQuery QueryArgs
  | CommandApply ApplyArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "query" $ CommandQuery <$> parseCommandQuery,
        OptParse.command "apply" $ CommandApply <$> parseCommandApply
      ]

data QueryArgs = QueryArgs
  { queryArgDeploymentFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

parseCommandQuery :: OptParse.ParserInfo QueryArgs
parseCommandQuery = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Query a deployment"
    parser = QueryArgs <$> optional parseDeploymentFileOption

data ApplyArgs = ApplyArgs
  { applyArgDeploymentFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

parseCommandApply :: OptParse.ParserInfo ApplyArgs
parseCommandApply = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Apply a deployment"
    parser = ApplyArgs <$> optional parseDeploymentFileOption

parseDeploymentFileOption :: OptParse.Parser FilePath
parseDeploymentFileOption =
  strOption
    ( mconcat
        [ long "deployment-file",
          help "Path to a deployment file",
          metavar "FILEPATH"
        ]
    )

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagStateFile :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "state-file",
                help "Path to an altenative state file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          (parseLogLevel <$> str)
          ( mconcat
              [ long "log-level",
                help "Minimal severity of log messages",
                metavar "FILEPATH"
              ]
          )
      )

parseLogLevel :: Text -> LogLevel
parseLogLevel = \case
  "Debug" -> LevelDebug
  "Info" -> LevelInfo
  "Warn" -> LevelWarn
  "Error" -> LevelError
  t -> LevelOther t

renderLogLevel :: LogLevel -> Text
renderLogLevel = \case
  LevelDebug -> "Debug"
  LevelInfo -> "Info"
  LevelWarn -> "Warn"
  LevelError -> "Error"
  LevelOther t -> t
