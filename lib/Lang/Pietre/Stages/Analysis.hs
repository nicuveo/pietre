{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Control.Lens                           hiding (mapping, op)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe              (hoistMaybe)
import Data.HashMap.Strict.Extra              qualified as M
import Data.HashSet                           qualified as S

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Symbol
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Core
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Monad

type DefinitionCache = HashMap Name (WithLocation (Definition Resolved))
type SymbolCache     = HashMap Name Symbol
type FunctionCache   = HashMap Name (Scope, WithLocation (FunctionInfo Parsed))

data ResolvedModule = ResolvedModule
  { _resmodExported    :: HashSet Identifier
  , _resmodDefinitions :: DefinitionCache
  , _resmodSymbols     :: SymbolCache
  , _resmodFunctions   :: FunctionCache
  }

makeLenses ''ResolvedModule


analyzeModule
  :: DefinitionCache
  -> SymbolCache
  -> FunctionCache
  -> HashMap ModuleName (HashSet Identifier)
  -> ModuleName
  -> Module
  -> ([Diagnostic], Maybe ResolvedModule)
analyzeModule
  foreignDefinitions
  _foreignSymbols
  _foreignFunctions
  moduleExports
  moduleName
  Module {..} = runMaybeT do
  (exported, localDefinitions, localScope) <- createLocalScope moduleName _modDefinitions
  foreignScope <- createForeignScope moduleExports _modImports
  let builtinScope = M.fromList $ map (fmap pure) builtins
  let topLevelScope = builtinScope `combineMaps` localScope `combineMaps` foreignScope
  let analysisInfo = AnalysisInfo moduleName S.empty localDefinitions foreignDefinitions topLevelScope
  let (diagnostics, resolvedModule) =
        runAnalysis analysisInfo do
          traverse_ analyzeDefinition _modDefinitions
          definitions <- use contextCache
          pure $ ResolvedModule exported (M.catMaybes definitions) M.empty M.empty
  tell diagnostics
  hoistMaybe resolvedModule
  where
    combineMaps = M.unionWith (<>)
