{-# LANGUAGE TemplateHaskell #-}

{-| Unittests for Attoparsec support for unicode -}

module Test.Idly.Attoparsec (testAttoparsec) where

import           Test.HUnit

import           Test.Idly.TestHelper

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Text            (pack, unpack)

-- | Unicode test string, first part.
part1 :: String
part1 = "äßĉ"

-- | Unicode test string, second part.
part2 :: String
part2 = "ðèق"

-- | Simple parser able to split a string in two parts, name and
-- value, separated by a '=' sign.
simpleParser :: Parser (String, String)
simpleParser = do
  n <- A.takeTill (\c -> A.isHorizontalSpace c || c == '=')
  A.skipWhile A.isHorizontalSpace
  _ <- A.char '='
  A.skipWhile A.isHorizontalSpace
  v <- A.takeTill A.isEndOfLine
  return (unpack n, unpack v)

{-# ANN case_unicodeParsing "HLint: ignore Use camelCase" #-}
-- | Tests whether a Unicode string is still Unicode after being
-- parsed.
case_unicodeParsing :: Assertion
case_unicodeParsing =
  case A.parseOnly simpleParser text of
    Right (name, value) -> do
      assertEqual "name part" part1 name
      assertEqual "value part" part2 value
    Left msg -> assertFailure $ "Failed to parse: " ++ msg
  where text = Data.Text.pack $ part1 ++ "  = \t" ++ part2

testSuite "Attoparsec"
          [ 'case_unicodeParsing
          ]
