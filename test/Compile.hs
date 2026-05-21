module Compile
  ( makeFileSystemFromFilesIn
  , parse
  , analyze
  , simplify
  , compile
  , runTest
  , runTestOrFail
  ) where

import "this" Prelude

import Data.HashMap.Strict                          qualified as Map
import Data.Text                                    qualified as Text
import Data.Text.IO                                 qualified as Text
import System.Directory                             as Directory
import System.FilePath
import Test.Tasty.HUnit

import Lang.Pietre.Export.PrettyPrinting.Diagnostic
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Pipeline.Compile
import Lang.Pietre.Pipeline.Monad
import Lang.Pietre.Pipeline.Options
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Image
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis
import Lang.Pietre.Stages.Parsing
import Lang.Pietre.Stages.Simplification


--------------------------------------------------------------------------------
-- Public API

newtype TestRun a = TestRun (ReaderT FilePath (DiagnosisT (State InMemoryFileSystem)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader FilePath
    , MonadState  InMemoryFileSystem
    , MonadDiagnosis
    )

type InMemoryFileSystem = HashMap FilePath Text

makeFileSystemFromFilesIn :: FilePath -> IO InMemoryFileSystem
makeFileSystemFromFilesIn folder = do
  contents <- listDirectory folder >>= traverse \name -> do
    let path = folder </> name
    isDir <- doesDirectoryExist path
    if isDir
    then pure Nothing
    else do
      fileContent <- Text.readFile path
      pure $ Just (path, fileContent)
  pure $ Map.fromList $ catMaybes contents



instance MonadFileSystem TestRun where
  doesFileExist  = gets     . Map.member
  readSourceFile = gets     . Map.lookup
  writeToFile    = modify ... Map.insert

parse :: Text -> TestRun Module
parse source = do
  filename <- ask
  parseModule filename source

analyze :: Module -> TestRun Interface
analyze parsedModule = do
  name <- moduleName
  analyzeModule
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    name
    parsedModule

simplify :: Interface -> TestRun Interface
simplify = pure . simplifyModule

compile :: TestRun Image
compile = do
  filename <- ask
  let
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
  compileBinary compilerOptions compilerFlags filename


runTest
  :: FilePath
  -> InMemoryFileSystem
  -> TestRun a
  -> ((Seq Diagnostic, Maybe a), InMemoryFileSystem)
runTest filename files (TestRun action) = action
  & flip runReaderT filename
  & runDiagnosisT
  & flip runState files

runTestOrFail
  :: FilePath
  -> InMemoryFileSystem
  -> TestRun a
  -> IO (a, InMemoryFileSystem)
runTestOrFail filename files action = do
  let
    ((diagnostics, resultValue), resultFiles) = runTest filename files action
    errorMessage = Text.unpack $ Text.unlines $ map prettyPrint $ toList diagnostics
  case resultValue of
    Nothing -> assertFailure ("COMPILATION FAILED:\n" <> errorMessage)
    Just x  -> pure (x, resultFiles)


--------------------------------------------------------------------------------
-- Local helpers

moduleName :: TestRun ModuleName
moduleName = do
  filename <- ask
  pure $ pure $ Identifier $ Text.pack $ takeBaseName filename
