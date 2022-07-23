{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module GhcidFileParser.CLI.Run.Command.Parse
  ( run

  , ParsedFilePath(..)
  , parseFilePath
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.Aeson (Message(..), MonadLogger, logDebug, logError, logInfo)
import Data.Aeson (KeyValue((.=)), ToJSON)
import Data.ByteString.Char8 (ByteString)
import GhcidFileParser.CLI.Command (InputSource(..), ParseCommand(..))
import Prelude
import System.Exit (ExitCode(..))
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified System.Directory as Directory
import qualified System.IO as IO

run :: (MonadLogger m, MonadIO m) => ParseCommand -> m ExitCode
run = \case
  ParseInput inputSource -> do
    input <- readInputContents inputSource
    logInfo $ "Parsing input" :#
      [ "input" .= Text.Encoding.decodeUtf8 input ]
    let inputNoLineBreaks = BS8.filter (not . isSpaceOtherThanSpaceChar) input
    logDebug $ "Input without line breaks" :#
      [ "inputNoLineBreaks" .= Text.Encoding.decodeUtf8 inputNoLineBreaks ]
    let inputWords = BS8.words inputNoLineBreaks
    logDebug $ "Input as words" :#
      [ "inputWords" .= fmap Text.Encoding.decodeUtf8 inputWords ]
    go inputWords >>= \case
      Nothing {} -> do
        logError $ "Failed to find a file path in input" :#
          [ "input" .= Text.Encoding.decodeUtf8 input ]
        pure $ ExitFailure 1
      Just {} -> do
        pure ExitSuccess
    where
    go = \case
      [] -> pure Nothing
      (w : ws) -> do
        case parseFilePath w of
          Left parseError -> do
            logDebug $ "Word is not a (parsable) file path" :#
              [ "inputWord" .= Text.Encoding.decodeUtf8 w
              , "parseError" .= parseError
              ]
            go ws
          Right parsedFilePath -> do
            fileExists <- liftIO $ Directory.doesFileExist $ file parsedFilePath
            if not fileExists then do
              logError $ "Parsed file path does not exist on file system" :#
                [ "inputWord" .= Text.Encoding.decodeUtf8 w
                , "parsedFilePath" .= parsedFilePath
                ]
              go ws
            else do
              logDebug $ "Parsed file path exists on file system" :#
                [ "parsedFilePath" .= parsedFilePath
                ]
              liftIO $ BSL8.hPutStrLn IO.stdout $ Aeson.encode parsedFilePath
              pure $ Just parsedFilePath

    isSpaceOtherThanSpaceChar c =
      Char.isSpace c && c /= ' '

    readInputContents = liftIO . \case
      InputSourceStdin -> BS8.getContents
      InputSourceFile filePath -> BS8.readFile filePath

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
