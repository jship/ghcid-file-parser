module GhcidFileParser.CLI.Parse.Common
  ( command'
  ) where

import Options.Applicative (CommandFields, Mod, Parser)
import Prelude
import qualified Options.Applicative as Opt

command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  Opt.command label
    $ Opt.info parser
    $ Opt.progDesc description
