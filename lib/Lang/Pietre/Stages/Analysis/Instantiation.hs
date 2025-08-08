module Lang.Pietre.Stages.Analysis.Instantiation where

import                "this" Prelude

import                Control.Lens                           hiding (mapping,
                                                              op, (...))
import                Data.HashMap.Strict                    qualified as M
import                Data.Set                               qualified as S

import                Lang.Pietre.Representations.AST
import                Lang.Pietre.Representations.Location
import                Lang.Pietre.Representations.Name
import                Lang.Pietre.Representations.Symbol     qualified as Symbol
import                Lang.Pietre.Representations.Tokens
import {-# SOURCE #-} Lang.Pietre.Stages.Analysis.Core
import                Lang.Pietre.Stages.Analysis.Diagnostic
import                Lang.Pietre.Stages.Analysis.Monad


isGeneric :: FunctionInfo p -> Bool
isGeneric = not . null . _funParams

tryInstantiateGenericFunction
  :: [Identifier]
  -> Name
  -> HashMap Identifier (PathInfo Resolved)
  -> AnalysisM (Maybe Name)
tryInstantiateGenericFunction params name mappings =
  if not (null params) && all isConcrete mappings && null (_nameParameters name)
  then do
    views infoForeignFunctions (M.lookup name) >>= \case
      Just (scope, info) -> do
        Just <$> instantiateGenericFunction scope name info mappings
      Nothing -> do
        moduleInstances %= S.insert (name, mappings)
        pure $ Just $ generateFullFunctionName name params mappings
  else
    pure Nothing
  where
    isConcrete PathInfo {..} = all isConcrete _pathParams && case _pathName of
      TopLevelDeclaration _ -> True
      BuiltinType         _ -> True
      _                     -> False

generateFullFunctionName
  :: Name
  -> [Identifier]
  -> HashMap Identifier (PathInfo Resolved)
  -> Name
generateFullFunctionName name paramNames mappings =
  go name do
    paramName <- paramNames
    M.lookup paramName mappings
      `onNothing` error "ICE"
  where
    go originalName typeParams = Name (_nameFullPath originalName) do
      PathInfo {..} <- typeParams
      pure $ case _pathName of
        BuiltinType         n -> go n _pathParams
        TopLevelDeclaration n -> go n _pathParams
        _                     -> error "ICE"

instantiateGenericFunction
  :: Scope
  -> Name
  -> WithLocation (FunctionInfo Parsed)
  -> HashMap Identifier (PathInfo Resolved)
  -> AnalysisM Name
instantiateGenericFunction topLevelScope name locatedDefinition params = do
  symbol <- views infoForeignSymbols (M.lookup fullName)
    `onNothingM` uses moduleSymbols (M.lookup fullName)
    `onNothingM` doAnalysis
    `onNothingM` abort
  moduleSymbols %= M.insert fullName symbol
  pure fullName
  where
    fullName = generateFullFunctionName name (_funParams $ _located locatedDefinition) params
    doAnalysis =
      withContext topLevelScope fullName (_location locatedDefinition) do
        currentParams .= params
        resolvedFunction <- analyzeFunction $ _located locatedDefinition
        pure $ Symbol.Function resolvedFunction


substituteTypes
  :: HashMap Identifier (PathInfo Resolved)
  -> PathInfo Resolved
  -> AnalysisM (PathInfo Resolved)
substituteTypes mappings info@PathInfo {..} = case _pathName of
  TypeParameter _ name -> M.lookup name mappings `onNothing` error "ICE"
  _                    -> pathParams (traverse $ substituteTypes mappings) info
