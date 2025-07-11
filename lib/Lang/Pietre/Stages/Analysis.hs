{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis
  ( DefinitionCache
  , ResolvedModule (..)
  , resmodExported
  , resmodDefinitionCache
  , analyzeModule
  ) where

import "this" Prelude

import Control.Lens                           hiding (mapping, op)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe              (hoistMaybe)
import Data.HashMap.Strict                    qualified as M
import Data.HashSet                           qualified as S

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Core
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Monad


type DefinitionCache = HashMap Name (WithLocation (Definition Resolved))

data ResolvedModule = ResolvedModule
  { _resmodExported        :: HashSet Identifier
  , _resmodDefinitionCache :: DefinitionCache
  }

makeLenses ''ResolvedModule

analyzeModule
  :: DefinitionCache
  -> HashMap ModuleName (HashSet Identifier)
  -> ModuleName
  -> Module
  -> ([Diagnostic], Maybe ResolvedModule)
analyzeModule foreignDefinitions moduleExports moduleName Module {..} = runMaybeT do
  (exported, localDefinitions, localScope) <- createLocalScope moduleName _modDefinitions
  foreignScope <- createForeignScope moduleExports _modImports
  let builtinScope = M.fromList $ map (fmap pure) builtins
  let topLevelScope = builtinScope `combineMaps` localScope `combineMaps` foreignScope
  let analysisInfo = AnalysisInfo moduleName S.empty localDefinitions foreignDefinitions topLevelScope
  let (diagnostics, resolvedModule) =
        runAnalysis analysisInfo do
          traverse_ (try . analyzeDefinition) _modDefinitions
          cachedDefinitions <- use contextCache
          pure $ ResolvedModule exported cachedDefinitions
  tell diagnostics
  hoistMaybe resolvedModule
  where
    combineMaps = M.unionWith (<>)
