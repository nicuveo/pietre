module Lang.Pietre.Stages.Analysis.Instantiation where

import                "this" Prelude

import                Control.Lens                           hiding (mapping,
                                                              op, (...))
import                Data.HashMap.Strict                    qualified as M
import                Data.Set                               qualified as S

import                Lang.Pietre.Internal.ICE
import                Lang.Pietre.Representations.AST
import                Lang.Pietre.Representations.Identifier
import                Lang.Pietre.Representations.Location
import                Lang.Pietre.Representations.Name
import {-# SOURCE #-} Lang.Pietre.Stages.Analysis.Core
import                Lang.Pietre.Stages.Analysis.Diagnostic
import                Lang.Pietre.Stages.Analysis.Monad


isGeneric :: FunctionInfo p -> Bool
isGeneric = not . null . _funParams . _funType

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
      `onNothing`
        reportICE
          "generic function instantiation"
          "no information for type parameter"
          [ "parameter name:   " ++ show paramName
          , "known parameters: " ++ show mappings
          ]
  where
    go originalName typeParams = Name (_nameFullPath originalName) do
      path@PathInfo {..} <- typeParams
      pure $ case _pathName of
        BuiltinType         n -> go n _pathParams
        TopLevelDeclaration n -> go n _pathParams
        _                     -> reportICE
          "generic function instantiation"
          "type parameter isn't concrete"
          [ "path: " ++ show path
          ]

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
  moduleDefinitions %= M.insert fullName
    (Just $ FunctionDef symbol <$ locatedDefinition)
  pure fullName
  where
    paramNames = _funParams $ _funType $ _located locatedDefinition
    fullName = generateFullFunctionName name paramNames params
    doAnalysis =
      withContext topLevelScope fullName (_location locatedDefinition) do
        currentParams .= params
        resolvedFunction <- analyzeFunction $ _located locatedDefinition
        pure resolvedFunction


substituteTypes
  :: HashMap Identifier (PathInfo Resolved)
  -> PathInfo Resolved
  -> PathInfo Resolved
substituteTypes mappings info@PathInfo {..} =
  case _pathName of
    TypeParameter _ name ->
      M.lookupDefault (missingParameterError name) name mappings
    _ ->
      info & pathParams %~ map (substituteTypes mappings)
  where
    missingParameterError name =
      reportICE
        "generic function instantiation"
        "no information for type parameter"
        [ "parameter name:   " ++ show name
        , "known parameters: " ++ show mappings
        ]
