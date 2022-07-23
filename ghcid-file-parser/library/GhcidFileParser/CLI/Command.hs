module GhcidFileParser.CLI.Command
  ( Command(..)

  , module GhcidFileParser.CLI.Command.Parse
  , module GhcidFileParser.CLI.Command.Clean
  ) where

import GhcidFileParser.CLI.Command.Parse
import GhcidFileParser.CLI.Command.Clean

data Command
  = Parse ParseCommand
  | Clean CleanCommand
