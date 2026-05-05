module Compile
  ( parse
  , analyze
  , simplify
  , runTest
  ) where

import "this" Prelude

import Data.HashMap.Strict                    qualified as M
import Data.Text                              qualified as T
import System.FilePath

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Pipeline.Monad
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Identifier
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

instance MonadFileSystem TestRun where
  doesFileExist  = gets     . M.member
  readSourceFile = gets     . M.lookup
  writeToFile    = modify ... M.insert

parse :: Text -> TestRun Module
parse source = do
  filename <- ask
  parseModule filename source

analyze :: Module -> TestRun Interface
analyze parsedModule = do
  name <- moduleName
  analyzeModule
    M.empty
    M.empty
    M.empty
    M.empty
    name
    parsedModule

simplify :: Interface -> TestRun Interface
simplify = pure . simplifyModule


runTest
  :: FilePath
  -> InMemoryFileSystem
  -> TestRun a
  -> (InMemoryFileSystem, Either String a)
runTest filename files (TestRun action) =
  let ((diagnostics, result), fileResult) = action
        & flip runReaderT filename
        & runDiagnosisT
        & flip runState files
  in  (fileResult,) $ case result of
        Nothing    -> Left $ show diagnostics
        Just value -> Right value


--------------------------------------------------------------------------------
-- Local helpers

moduleName :: TestRun ModuleName
moduleName = do
  filename <- ask
  pure $ pure $ Identifier $ T.pack $ takeBaseName filename
