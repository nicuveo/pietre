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
  , MonadDiagnosis m
  )

parse :: TestCompiler m => Text -> m Module
parse source = do
  filename <- ask
  parseModule filename source

analyze :: TestCompiler m => Module -> m Interface
analyze parsedModule = do
  name <- moduleName
  analyzeModule
    M.empty
    M.empty
    M.empty
    M.empty
    name
    parsedModule

simplify :: TestCompiler m => Interface -> m Interface
simplify = pure . simplifyModule


runTestCompiler
  :: FilePath
  -> ReaderT FilePath Diagnosis a
  -> Either String a
runTestCompiler filename action =
  let (diagnostics, result) = runDiagnosis (runReaderT action filename)
  in  case result of
        Nothing    -> Left $ show diagnostics
        Just value -> Right value


--------------------------------------------------------------------------------
-- Local helpers

moduleName :: TestCompiler m => m ModuleName
moduleName = do
  filename <- ask
  pure $ pure $ Identifier $ T.pack $ takeBaseName filename
