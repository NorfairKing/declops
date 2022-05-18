{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Declops.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Maybe
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
  { settingStateFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

data Dispatch
  = DispatchApply !ApplySettings
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
  dispatch <- case cmd of
    CommandApply ApplyArgs {..} -> do
      applySettingDeploymentFile <-
        resolveFile' $
          fromMaybe "deployment.nix" $
            applyArgDeploymentFile <|> envDeploymentFile <|> (mConf >>= configDeploymentFile)
      pure $ DispatchApply ApplySettings {..}
  pure $ Instructions dispatch Settings {..}

data Configuration = Configuration
  { configDeploymentFile :: !(Maybe FilePath),
    configStateFile :: !(Maybe FilePath)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "deployment-file" "deployment specification" .= configDeploymentFile
        <*> optionalField "state-file" "where to store deployment state" .= configStateFile

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
    envStateFile :: !(Maybe FilePath)
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
      <*> Env.var (fmap Just . Env.str) "STATE_FILE" (Env.def Nothing <> Env.help "Config file")

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments Command Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandApply ApplyArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "apply" $ CommandApply <$> parseCommandApply
      ]

-- | One type per command, for the command-specific arguments
data ApplyArgs = ApplyArgs
  { applyArgDeploymentFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

-- | One 'optparse-applicative' parser for each command's flags
parseCommandApply :: OptParse.ParserInfo ApplyArgs
parseCommandApply = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Apply the user"
    parser =
      ApplyArgs
        <$> optional
          ( strOption
              ( mconcat
                  [ long "deployment-file",
                    help "Path to a deployment file",
                    metavar "FILEPATH"
                  ]
              )
          )

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagStateFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
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
