module GhcidFileParser.CLI.Parse.Command.Clean
  ( parse
  ) where

import GhcidFileParser.CLI.Command (CleanCommand(..), Command(..))
import GhcidFileParser.CLI.Parse.Common (command')
import Options.Applicative (CommandFields, Mod)
import Prelude

parse :: Mod CommandFields Command
parse = do
  command' "clean" "Clean up log files" go
  where
  go = pure $ Clean $ CleanLogFiles
