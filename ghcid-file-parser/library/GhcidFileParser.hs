{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
module GhcidFileParser
  ( main
  , ParsedFilePath(..)
  , parseFilePath
  ) where

import Data.Aeson (ToJSON)
import Data.ByteString.Char8 (ByteString)
import GHC.Generics (Generic)
import Prelude
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified System.Exit as Exit
import qualified System.IO as IO

data ParsedFilePath = ParsedFilePath
  { file :: FilePath
  , line :: Int
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

main :: IO ()
main = do
  firstLine <- BS8.getLine
  case parseFilePath firstLine of
    Left err -> do
      IO.hPutStrLn IO.stderr $ "Failed to parse file path from 'ghcid': " <> err
      Exit.exitFailure
    Right parsedFilePath -> do
      BSL8.hPutStrLn IO.stdout $ Aeson.encode parsedFilePath

parseFilePath :: ByteString -> Either String ParsedFilePath
parseFilePath =
  Attoparsec.parseOnly do
    Attoparsec.skipSpace
    file <- do
      fmap (Text.unpack . Text.Encoding.decodeUtf8) do
        Attoparsec.takeWhile1 (/= ':')
    Monad.void $ Attoparsec.char ':'
    Attoparsec.skipWhile (== '(')
    line <- Attoparsec.decimal
    pure ParsedFilePath { file, line }
