{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module E2E.Examples
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Control.Monad.Extra          (ifM)
import Data.HashMap.Strict          qualified as Map
import Data.Text                    qualified as Text
import Data.Text.IO                 qualified as Text
import Graphics.Image               qualified as Img
import System.Directory             as Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty.HUnit

import Lang.Pietre.Pipeline.Compile
import Lang.Pietre.Pipeline.Options

import Compile
import Locate


test_batch = do
  testFolder <- $(listFolders "test/E2E/examples")
  let
    testName    = takeBaseName testFolder
    mainFile    = testFolder </> "main.pi"
    stdinFile   = testFolder </> "test.in"
    stdoutFile  = testFolder </> "test.out"
    compilerOptions = CompilerOptions
      { _coVerbose         = False
      , _coJSONDiagnostics = True
      , _coIncludePaths    = pure "."
      , _coExportAST       = Nothing
      , _coExportIR        = Nothing
      , _coExportBytecode  = Nothing
      , _coExportBinary    = Nothing
      , _coOutput          = Nothing
      }
    compilerFlags = CompilerFlags
      { _cfSimplify = True
      , _cfOptimize = True
      , _cfMinimize = True
      }
  pure $ testCase testName do
    programName <- getTemporaryDirectory <&> (</> "program.png")
    inMemoryFileSystem <-
      fmap (Map.fromList . catMaybes) $
      listDirectory testFolder >>= traverse \name -> do
        let path = testFolder </> name
        isDir <- doesDirectoryExist path
        if isDir || takeExtension name /= ".pi"
        then pure Nothing
        else do
          fileContent <- Text.readFile path
          pure $ Just (path, fileContent)
    let result = snd
          $ runTest mainFile inMemoryFileSystem
          $ compileBinary compilerOptions compilerFlags mainFile
    image <- result `onLeft` \errMsg -> assertFailure $ "COMPILATION FAILED: " ++ errMsg
    Img.writeImageExact Img.PNG [] programName image
    stdin  <- Text.unpack <$> readFileIfExists stdinFile
    stdout <- Text.unpack <$> readFileIfExists stdoutFile
    (exitCode, out, _) <- readProcessWithExitCode "npiet" ["-q", programName] stdin
    exitCode @?= ExitSuccess
    out      @?= stdout

readFileIfExists :: FilePath -> IO Text
readFileIfExists filename = do
  ifM (Directory.doesFileExist filename) (Text.readFile filename) (pure "")
