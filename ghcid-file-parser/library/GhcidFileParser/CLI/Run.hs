{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcidFileParser.CLI.Run
  ( exec
  ) where

import Control.Exception.Safe (SomeException(..), catchAsync, displayException, throwM)
import Control.Monad.Logger.Aeson (Message(..), (.=), logError)
import GhcidFileParser.CLI.Command (Command)
import GhcidFileParser.CLI.Config (Config(..))
import Prelude
import System.FilePath ((<.>), (</>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Logger.Aeson as Logger
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified GhcidFileParser.CLI.Run.Command as Command
import qualified System.Directory as Directory
import qualified System.Exit as Exit

exec :: Config -> Command -> IO ()
exec config command = do
  ensureLogDirExists logDir
  logFilePath <- mkLogFilePath logDir
  exitCode <- Logger.runFileLoggingT logFilePath do
    Logger.filterLogger (const (>= minLogLevel)) do
      Command.run config logFilePath command `catchAsync` \(SomeException e) -> do
        logError $ "Encountered unhandled exception" :#
          [ "exception" .= displayException e
          ]
        throwM e
  logFileSize <- Directory.getFileSize logFilePath
  Monad.when (logFileSize == 0) do
    Directory.removeFile logFilePath
  Exit.exitWith exitCode
  where
  Config { minLogLevel, logDir } = config

ensureLogDirExists :: FilePath -> IO ()
ensureLogDirExists logDir = do
  Directory.createDirectoryIfMissing True logDir
  logDirPerms <- Directory.getPermissions logDir
  Directory.setPermissions logDir
    $ Directory.setOwnerReadable True
    $ Directory.setOwnerWritable True
    $ Directory.setOwnerExecutable True logDirPerms

mkLogFilePath :: FilePath -> IO FilePath
mkLogFilePath logDir = do
  localTime <- fmap Time.zonedTimeToLocalTime Time.getZonedTime
  pure $ logDir </> ISO8601.formatShow format localTime <.> "log"
  where
  format =
    ISO8601.localTimeFormat
      (ISO8601.calendarFormat ISO8601.BasicFormat)
      (ISO8601.timeOfDayFormat ISO8601.BasicFormat)
