{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module GhcidFileParser.CLI.Parse.Config
  ( parse
  ) where

import Control.Monad.Logger.Aeson (LogLevel(..))
import GhcidFileParser.CLI.Config (Config(..), defaultLogDir, defaultMinLogLevel)
import Options.Applicative (Parser)
import Prelude
import qualified Options.Applicative as Opt

parse :: Parser Config
parse = do
  minLogLevel <- logLevelOption
  logDir <- logDirOption
  pure Config { minLogLevel, logDir }
  where
  logLevelOption = Opt.option readLogLevel $ mconcat
    [ Opt.short 'l'
    , Opt.long "log-level"
    , Opt.help "One of [\"debug\", \"info\", \"warn\", \"error\"]"
    , Opt.value defaultMinLogLevel
    , Opt.showDefault
    , Opt.metavar "LOG_LEVEL"
    ]
  logDirOption = Opt.strOption $ mconcat
    [ Opt.short 'd'
    , Opt.long "log-dir"
    , Opt.help "Log directory"
    , Opt.value defaultLogDir
    , Opt.showDefault
    , Opt.metavar "LOG_DIR"
    ]
  readLogLevel = Opt.eitherReader \case
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "error" -> Right LevelError
    invalid ->
      Left $ "Failed to parse log level from one of "
        <> "[\"debug\", \"info\", \"warn\", \"error\"], got: " <> invalid
