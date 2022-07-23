{-# LANGUAGE NamedFieldPuns #-}
module GhcidFileParser.CLI.Main
  ( main
  ) where

import GhcidFileParser.CLI.Invocation (Invocation(..))
import Prelude
import System.IO (BufferMode(..))
import qualified GhcidFileParser.CLI.Parse as Parse
import qualified GhcidFileParser.CLI.Run as Run
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  Invocation { config, command } <- Parse.exec
  Run.exec config command
