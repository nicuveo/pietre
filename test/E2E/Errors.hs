{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module E2E.Errors
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.Aeson
import Data.Aeson.Key                     qualified as Key
import Data.Aeson.KeyMap                  (KeyMap)
import Data.Aeson.KeyMap                  qualified as KeyMap
import System.FilePath
import Test.Tasty.HUnit

import Lang.Pietre.Export.JSON.Diagnostic

import Compile
import Locate


test_batch = do
  testFolder <- $(listFolders "test/E2E/errors")
  let
    testName    = takeBaseName testFolder
    mainFile    = testFolder </> "main.pi"
    stdoutFile  = testFolder </> "compile.out"
  pure $ testCase testName do
    inMemoryFileSystem <- makeFileSystemFromFilesIn testFolder
    let ((diagnostics, _), _) = runTest mainFile inMemoryFileSystem compile
    expected <- readJSONValue stdoutFile
    matchValue expected (serialize diagnostics)


matchValue :: Value -> Value -> Assertion
matchValue = go ["$"]
  where
    go :: [String] -> Value -> Value -> Assertion
    go path = curry \case
      (Object expected, Object actual) -> matchObject path expected actual
      (Array  expected, Array  actual) -> matchArray  path (toList expected) (toList actual)
      (String expected, String actual) -> assertMatch path expected actual
      (Number expected, Number actual) -> assertMatch path expected actual
      (Bool   expected, Bool   actual) -> assertMatch path expected actual
      (Null,            Null         ) -> pass
      (expected,        actual       ) -> (assertMatch path `on` description) expected actual

    matchArray :: [String] -> [Value] -> [Value] -> Assertion
    matchArray path expected actual = do
      let renderLength = ("length " ++) . show . length
      (assertMatch path `on` renderLength) expected actual
      sequence_ $ zipWith3 (matchArrayValue path) [0..] expected actual

    matchArrayValue :: [String] -> Int -> Value -> Value -> Assertion
    matchArrayValue path index = go (show index : path)

    matchObject :: [String] -> KeyMap Value -> KeyMap Value -> Assertion
    matchObject path expectedObject actualObject =
      void $ flip KeyMap.traverseWithKey expectedObject \key expectedValue -> do
        actualValue <- KeyMap.lookup key actualObject `onNothing`
          assertFailure ("at " ++ renderPath path ++ ": missing key " ++ Key.toString key)
        go (Key.toString key : path) expectedValue actualValue

    description :: Value -> String
    description = \case
      Object _ -> "an object"
      Array  _ -> "an array"
      String _ -> "a string"
      Number _ -> "a number"
      Bool   _ -> "a boolean"
      Null     -> "null"

    renderPath :: [String] -> String
    renderPath = concat . intersperse "." . reverse

    assertMatch :: (Show a, Eq a) => [String] -> a -> a -> Assertion
    assertMatch path expected actual = assertMatchWith errMsg path expected actual
      where
        errMsg = "expected " ++ show expected ++ " but got " ++ show actual

    assertMatchWith :: (Eq a) => String -> [String] -> a -> a -> Assertion
    assertMatchWith errMsg path expected actual = do
      when (expected /= actual) do
        assertFailure $ "at " ++ renderPath path ++ ": " ++ errMsg

readJSONValue :: FilePath -> IO Value
readJSONValue filename =
  decodeFileStrict filename `onNothingM` assertFailure "COULD NOT DECODE EXPECTED DIAGNOSTICS"
