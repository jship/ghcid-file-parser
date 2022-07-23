{-# LANGUAGE LambdaCase #-}
module GhcidFileParser.CLI.Run.Command
  ( run
  ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger.Aeson (MonadLogger)
import GhcidFileParser.CLI.Command (Command(..))
import GhcidFileParser.CLI.Config (Config)
import Prelude
import System.Exit (ExitCode)
import qualified GhcidFileParser.CLI.Run.Command.Clean as Clean
import qualified GhcidFileParser.CLI.Run.Command.Parse as Parse

run
  :: (MonadCatch m, MonadLogger m, MonadIO m)
  => Config
  -> FilePath
  -> Command
  -> m ExitCode
run config logFilePath = \case
  Parse parseCommand -> Parse.run parseCommand
  Clean cleanCommand -> Clean.run config logFilePath cleanCommand
