{-# LANGUAGE BlockArguments #-}
module GhcidFileParser.CLI.Parse.Command.Parse
  ( parse
  ) where

import Control.Applicative (Alternative((<|>)))
import GhcidFileParser.CLI.Command (Command(..), InputSource(..), ParseCommand(..))
import GhcidFileParser.CLI.Parse.Common (command')
import Options.Applicative (CommandFields, Mod)
import Prelude
import qualified Options.Applicative as Opt

parse :: Mod CommandFields Command
parse = do
  command' "parse" "Parse first filepath from input" go
  where
  go = flip fmap parser \inputSource -> Parse $ ParseInput inputSource
  parser = inputSourceParser
  inputSourceParser = stdinInputSourceParser <|> fileInputSourceParser
  stdinInputSourceParser =
    Opt.flag' InputSourceStdin $ mconcat
      [ Opt.long "stdin"
      , Opt.help "Read from stdin"
      ]
  fileInputSourceParser =
    fmap InputSourceFile $ Opt.strOption $ mconcat
      [ Opt.short 'f'
      , Opt.long "file"
      , Opt.metavar "FILEPATH"
      , Opt.help "Read from file"
      ]
