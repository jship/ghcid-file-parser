{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcidFileParser.CLI.Run.Command.Clean
  ( run
  ) where

import Control.Exception.Safe (Exception(displayException), SomeException(..), MonadCatch, catchAny)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.Aeson (Message(..), (.=), MonadLogger, logDebug, logError)
import GhcidFileParser.CLI.Command (CleanCommand(..))
import GhcidFileParser.CLI.Config (Config(..))
import Prelude
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified System.Directory as Directory

run
  :: (MonadCatch m, MonadLogger m, MonadIO m)
  => Config
  -> FilePath
  -> CleanCommand
  -> m ExitCode
run config logFilePath = \case
  CleanLogFiles -> do
    fileNames <- liftIO $ Directory.listDirectory logDir
    go False fileNames >>= \case
      True -> do
        pure $ ExitFailure 1
      False -> do
        pure ExitSuccess
  where
  go encounteredError [] = pure encounteredError
  go encounteredError (fileName : rest) = do
      let filePath = logDir </> fileName
      if logFilePath == filePath then do
        logDebug $ "Will not remove log file currently in use" :#
          [ "logFilePath" .= logFilePath
          ]
        go encounteredError rest
      else do
        encounteredError' <- do
          liftIO (encounteredError <$ Directory.removeFile filePath)
            `catchAny` \(SomeException e) -> do
              logError $ "Failed to remove log file" :#
                [ "filePath" .= filePath
                , "exception" .= displayException e
                ]
              pure True
        go encounteredError' rest

  Config { logDir } = config
