module Compile
  ( parse
  , analyze
  , simplify
  , runTestCompiler
  ) where

import "this" Prelude

import Data.HashMap.Strict                    qualified as M
import Data.Text                              qualified as T
import System.FilePath

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis
import Lang.Pietre.Stages.Parsing
import Lang.Pietre.Stages.Simplification


--------------------------------------------------------------------------------
-- Public API

type TestCompiler m =
  ( MonadReader FilePath m
  , MonadError String m
  )

parse :: TestCompiler m => Text -> m Module
parse source = do
  filename <- ask
  parseModule filename source `onLeft` (throwError . show)

analyze :: TestCompiler m => Module -> m Interface
analyze parsedModule = do
  name <- moduleName
  (diagnostics, result) <- runDiagnosisT $
    analyzeModule
      M.empty
      M.empty
      M.empty
      M.empty
      name
      parsedModule
  result `onNothing` throwError (show diagnostics)

simplify :: TestCompiler m => Interface -> m Interface
simplify = pure . simplifyModule


runTestCompiler
  :: FilePath
  -> ReaderT FilePath (ExceptT String m) a
  -> m (Either String a)
runTestCompiler filename action = runExceptT (runReaderT action filename)


--------------------------------------------------------------------------------
-- Local helpers

moduleName :: TestCompiler m => m ModuleName
moduleName = do
  filename <- ask
  pure $ pure $ Identifier $ T.pack $ takeBaseName filename
