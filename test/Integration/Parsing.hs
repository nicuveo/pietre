{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module Integration.Parsing
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.Text.IO                                 qualified as T
import Data.Text.Lazy                               qualified as T
import Data.Text.Lazy.Encoding                      qualified as T
import System.FilePath
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import Lang.Pietre.Export.PrettyPrinting.AST.Parsed
import Lang.Pietre.Representations.AST.Parsed

import Arbitrary                                    ()
import Compile
import Locate


test_batch = do
  testInputFile <- $(listFiles "test/Integration/parsing" ".pi")
  let
    testName = takeBaseName testInputFile
    goldenFile = testInputFile -<.> "golden"
  pure $ goldenVsString testName goldenFile do
    source <- T.readFile testInputFile
    parsedModule <- parseModule testInputFile source
    pure
      $ T.encodeUtf8
      $ T.fromStrict
      $ prettyPrint
      $ parsedModule

test_prop :: Module -> Property
test_prop "round-trip" m = ioProperty do
  let print1 = prettyPrint m
  print2 <- prettyPrint <$> parseModule "" print1
  pure $ print1 === print2


parseModule
  :: FilePath
  -> Text
  -> IO Module
parseModule filePath sourceCode =
  fmap fst $ runTestOrFail filePath mempty $ parse sourceCode
