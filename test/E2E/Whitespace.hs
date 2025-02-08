{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module E2E.Whitespace
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.Text             qualified as T
import Data.Text.IO          qualified as T
import System.FilePath       (takeBaseName)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Arbitrary
import Compile
import Locate


compressWhitespace :: Text -> Text
compressWhitespace = T.unwords . T.words

test_batch = do
  file <- $(listFiles "test/E2E/whitespace" ".pi")
  pure $ testCase (takeBaseName file) do
    source <- T.readFile file
    compile source @=? compile (compressWhitespace source)

test_prop :: SourceFile -> Property
test_prop "arbitrary file" (SourceFile content) = compile content === compile (compressWhitespace content)
