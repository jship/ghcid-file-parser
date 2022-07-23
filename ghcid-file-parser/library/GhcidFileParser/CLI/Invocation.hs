module GhcidFileParser.CLI.Invocation
  ( Invocation(..)
  ) where

import GhcidFileParser.CLI.Command (Command)
import GhcidFileParser.CLI.Config (Config)

data Invocation = Invocation
  { config :: Config
  , command :: Command
  }
