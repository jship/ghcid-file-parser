{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module GhcidFileParser.CLI.Parse.Invocation
  ( parse
  ) where

import GhcidFileParser.CLI.Invocation (Invocation(..))
import Options.Applicative (Parser)
import Prelude
import qualified GhcidFileParser.CLI.Parse.Command as Command
import qualified GhcidFileParser.CLI.Parse.Config as Config
import qualified Options.Applicative as Opt

parse :: Parser Invocation
parse = do
  config <- Config.parse
  command <- Command.parse
  helper <- Opt.helper
  pure $ helper Invocation { config, command }
