module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Data.Sequence                          qualified as Seq

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Resolution
import Lang.Pietre.Stages.Analysis.Validation


analyzeModule
  :: MonadDiagnosis m
  => HashMap ModuleName Interface
  -> DefinitionCache
  -> FunctionCache
  -> SymbolCache
  -> ModuleName
  -> Module
  -> m Interface
analyzeModule
  dependencies
  definitionCache
  functionCache
  symbolCache
  moduleName
  moduleInfo
  = do
  (moduleExports, resolvedDefinitions) <-
    resolve dependencies moduleName moduleInfo
  (moduleDefinitions, moduleFunctions, moduleSymbols) <-
    validate definitionCache functionCache symbolCache resolvedDefinitions
  pure $ Interface
    { _interfaceDependencies = Seq.fromList $ map _located $ _modImports moduleInfo
    , _interfaceExported     = moduleExports
    , _interfaceDefinitions  = moduleDefinitions
    , _interfaceFunctions    = moduleFunctions
    , _interfaceSymbols      = moduleSymbols
    }
