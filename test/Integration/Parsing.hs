{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module Integration.Parsing
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.Text.IO            qualified as T
import Data.Text.Lazy          qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Lang.Pietre
import System.FilePath
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Arbitrary               ()
import Locate


test_batch = do
  testInputFile <- $(listFiles "test/Integration/parsing" ".pi")
  let
    testName = takeBaseName testInputFile
    goldenFile = testInputFile -<.> "golden"
  pure $ goldenVsString testName goldenFile do
    source <- T.readFile testInputFile
    ast <- parseModule testInputFile source
      `onLeft` (assertFailure . show)
    pure
      $ T.encodeUtf8
      $ T.fromStrict
      $ prettyPrint ast

test_prop :: Module -> Property
test_prop "round-trip" m =
  let print1 = prettyPrint m
      print2 = prettyPrint <$> parseModule "" print1
  in  Right print1 === print2
