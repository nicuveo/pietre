{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Control.Lens                              hiding (mapping, op)
import Control.Monad.Loops                       (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                 (hoistMaybe)
import Data.HashMap.Strict.Extra                 qualified as M
import Data.HashSet                              qualified as S
import Data.Set                                  qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Core
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Instantiation
import Lang.Pietre.Stages.Analysis.Monad


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
  foreignSymbols
  foreignFunctions
  moduleExports
  moduleName
  Module {..} = runMaybeT do
    (exported, localDefinitions, localScope) <- createLocalScope moduleName _modDefinitions
    foreignScope <- createForeignScope moduleExports _modImports
    let builtinScope = M.fromList $ map (fmap pure) builtins
        topLevelScope = builtinScope `combineMaps` localScope `combineMaps` foreignScope
        analysisInfo = AnalysisInfo
          moduleName
          S.empty
          localDefinitions
          foreignDefinitions
          foreignSymbols
          foreignFunctions
          topLevelScope
        (diagnostics, resolvedModule) =
          runAnalysis analysisInfo do
            traverse_ analyzeDefinition _modDefinitions
            whileJust (uses moduleInstances Set.minView) \((name, params), remainingInstances) -> do
              moduleInstances .= remainingInstances
              (_, functionDefinition) <- uses moduleFunctions (M.lookup name)
                `onNothingM`
                  reportICE
                    "function instantiation"
                    "function definition not found"
                    ["name: " ++ show name]
              instantiateGenericFunction topLevelScope name functionDefinition params
            definitions <- use moduleDefinitions
            functions   <- use moduleFunctions
            symbols     <- use moduleSymbols
            pure $ ResolvedModule
              { _resmodExported    = exported
              , _resmodDefinitions = M.catMaybes definitions
              , _resmodSymbols     = symbols
              , _resmodFunctions   = functions
              }
    tell diagnostics
    hoistMaybe resolvedModule
  where
    combineMaps = M.unionWith (<>)
