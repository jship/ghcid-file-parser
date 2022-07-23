module GhcidFileParser.CLI.Parse
  ( exec
  ) where

import GhcidFileParser.CLI.Invocation (Invocation)
import Prelude
import qualified GhcidFileParser.CLI.Parse.Invocation as Invocation
import qualified Options.Applicative as Opt

exec :: IO Invocation
exec = Opt.execParser $ Opt.info Invocation.parse $ mconcat $
  [ Opt.fullDesc
  , Opt.progDesc "GhcidFileParser"
  ]
