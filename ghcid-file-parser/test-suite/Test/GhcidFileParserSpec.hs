{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.GhcidFileParserSpec
  ( spec
  ) where

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Char8 (ByteString)
import GHC.Stack (HasCallStack)
import GhcidFileParser (ParsedFilePath(..), parseFilePath)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Aeson as Aeson

spec :: Spec
spec = do
  describe "Ghcid.File.Parser" do
    it "works" do
      runTest TestCase
        { inputLine = "ghcid-file-parser/library/GhcidFileParser.hs:27:3-33: error:"
        , expectedParsedFilePath =
            Right ParsedFilePath
              { file = "ghcid-file-parser/library/GhcidFileParser.hs"
              , line = 27
              }
        , expectedValue =
            Just [aesonQQ|
              {
                "file": "ghcid-file-parser/library/GhcidFileParser.hs",
                "line": 27
              }
            |]
        }

      runTest TestCase
        { inputLine = "ghcid-file-parser/library/GhcidFileParser.hs:(211,1)-(216,31): warning: [-Wredundant-constraints]"
        , expectedParsedFilePath =
            Right ParsedFilePath
              { file = "ghcid-file-parser/library/GhcidFileParser.hs"
              , line = 211
              }
        , expectedValue =
            Just [aesonQQ|
              {
                "file": "ghcid-file-parser/library/GhcidFileParser.hs",
                "line": 211
              }
            |]
        }

      runTest TestCase
        { inputLine = "ghcid-file-parser/library/GhcidFileParser.hs:42-43: (fake) warning:"
        , expectedParsedFilePath =
            Right ParsedFilePath
              { file = "ghcid-file-parser/library/GhcidFileParser.hs"
              , line = 42
              }
        , expectedValue =
            Just [aesonQQ|
              {
                "file": "ghcid-file-parser/library/GhcidFileParser.hs",
                "line": 42
              }
            |]
        }

      runTest TestCase
        { inputLine = "ghcid-file-parser/library/GhcidFileParser.hs"
        , expectedParsedFilePath =
            Left ":: not enough input"
        , expectedValue = Nothing
        }

runTest :: (HasCallStack) => TestCase -> IO ()
runTest testCase = do
  parseFilePath inputLine `shouldBe` expectedParsedFilePath
  case parseResult of
    Left _err -> pure ()
    Right parsedFilePath -> do
      Just (Aeson.toJSON parsedFilePath) `shouldBe` expectedValue
  where
  parseResult = parseFilePath inputLine
  TestCase
    { inputLine
    , expectedParsedFilePath
    , expectedValue
    } = testCase

data TestCase = TestCase
  { inputLine :: ByteString
  , expectedParsedFilePath :: Either String ParsedFilePath
  , expectedValue :: Maybe Value -- ^ Use 'Nothing' when expecting a parse error
  }
