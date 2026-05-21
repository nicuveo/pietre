{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module E2E.Examples
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Control.Monad.Extra (ifM)
import Data.Text           qualified as Text
import Data.Text.IO        qualified as Text
import Graphics.Image      qualified as Img
import System.Directory    as Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty.HUnit

import Compile
import Locate


test_batch = do
  testFolder <- $(listFolders "test/E2E/examples")
  let
    testName    = takeBaseName testFolder
    mainFile    = testFolder </> "main.pi"
    stdinFile   = testFolder </> "test.in"
    stdoutFile  = testFolder </> "test.out"
  pure $ testCase testName do
    programName <- getTemporaryDirectory <&> (</> "program.png")
    inMemoryFileSystem <- makeFileSystemFromFilesIn testFolder
    (image, _) <- runTestOrFail mainFile inMemoryFileSystem compile
    Img.writeImageExact Img.PNG [] programName image
    stdin  <- Text.unpack <$> readFileIfExists stdinFile
    stdout <- Text.unpack <$> readFileIfExists stdoutFile
    (exitCode, out, _) <- readProcessWithExitCode "npiet" ["-q", programName] stdin
    exitCode @?= ExitSuccess
    out      @?= stdout

readFileIfExists :: FilePath -> IO Text
readFileIfExists filename = do
  ifM (Directory.doesFileExist filename) (Text.readFile filename) (pure "")
