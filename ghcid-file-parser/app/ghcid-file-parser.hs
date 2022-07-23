{-# LANGUAGE BlockArguments #-}
module Main
  ( main
  ) where

import qualified GhcidFileParser.CLI
import Prelude

main :: IO ()
main = GhcidFileParser.CLI.main
