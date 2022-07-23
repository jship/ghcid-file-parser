{-# LANGUAGE BlockArguments #-}
module GhcidFileParser.CLI.Config
  ( Config(..)
  , defaultMinLogLevel
  , defaultLogDir
  ) where

import Control.Monad.Logger.Aeson (LogLevel(..))
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Directory as Directory

data Config = Config
  { minLogLevel :: LogLevel
  , logDir :: FilePath
  }

defaultMinLogLevel :: LogLevel
defaultMinLogLevel = LevelError

defaultLogDir :: FilePath
defaultLogDir =
  unsafePerformIO do
    Directory.getXdgDirectory Directory.XdgCache "ghcid-file-parser"
{-# NOINLINE defaultLogDir #-}
