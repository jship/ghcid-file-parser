{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcidFileParser
  ( main
  , ParsedFilePath(..)
  , parseFilePath
  ) where

import Data.Aeson (KeyValue((.=)), ToJSON)
import Data.ByteString.Char8 (ByteString)
import Prelude
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO

data ParsedFilePath = ParsedFilePath
  { file :: FilePath
  , line :: Int
  , char :: Int
  } deriving stock (Eq, Show)

instance ToJSON ParsedFilePath where
  toJSON ParsedFilePath { file, line, char } =
    Aeson.object [ "file" .= file, "line" .= line, "char" .= char ]

  toEncoding ParsedFilePath { file, line, char } =
    Aeson.pairs $ mconcat [ "file" .= file, "line" .= line, "char" .= char ]

main :: IO ()
main = do
  firstLine <- BS8.getLine
  case parseFilePath firstLine of
    Left err -> do
      IO.hPutStrLn IO.stderr $ "Failed to parse file path from 'ghcid': " <> err
      Exit.exitFailure
    Right parsedFilePath -> do
      fileExists <- Directory.doesFileExist $ file parsedFilePath
      Monad.when fileExists do
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
    Monad.void $ Attoparsec.choice [Attoparsec.char ':', Attoparsec.char ',']
    char <- Attoparsec.decimal
    pure ParsedFilePath { file, line, char }
