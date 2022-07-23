module GhcidFileParser.CLI.Command.Parse
  ( ParseCommand(..)
  , InputSource(..)
  ) where

import Prelude

data ParseCommand
  = ParseInput InputSource

data InputSource
  = InputSourceStdin
  | InputSourceFile FilePath
