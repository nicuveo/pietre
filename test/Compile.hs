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

import Lang.Pietre
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Name


--------------------------------------------------------------------------------
-- Public API

type TestCompiler m = (MonadReader FilePath m, MonadError String m)

parse :: TestCompiler m => Text -> m Module
parse source = do
  filename <- ask
  parseModule filename source `onLeft` (throwError . show)

analyze :: TestCompiler m => Module -> m ResolvedModule
analyze parsedModule = do
  name <- moduleName
  let (diagnostics, result) = analyzeModule M.empty M.empty M.empty M.empty name parsedModule
  result `onNothing` throwError (show diagnostics)

simplify :: TestCompiler m => ResolvedModule -> m ResolvedModule
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
