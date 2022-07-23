module GhcidFileParser.CLI.Parse.Command
  ( parse
  ) where

import GhcidFileParser.CLI.Command (Command)
import Options.Applicative (Parser)
import Prelude
import qualified GhcidFileParser.CLI.Parse.Command.Parse as Command.Parse
import qualified GhcidFileParser.CLI.Parse.Command.Clean as Command.Clean
import qualified Options.Applicative as Opt

parse :: Parser Command
parse = Opt.hsubparser parsers
  where
  parsers =
    mconcat
      [ Command.Parse.parse
      , Command.Clean.parse
      ]
