module Lang.Pietre.Stages.Analysis.Resolution.Scope where

import "this" Prelude

import Control.Lens                                          hiding (cons,
                                                              mapping, op)
import Data.HashMap.Strict.Extra                             qualified as M
import Data.List.NonEmpty                                    qualified as NE

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Parsed                as Parsed
import Lang.Pietre.Representations.AST.Resolved              as Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Resolution.Monad
import Lang.Pietre.Stages.Analysis.Resolution.NameValidation


--------------------------------------------------------------------------------
-- Scope creation

createImportedScope
  :: MonadDiagnosis m
  => HashMap ModuleName Interface
  -> [WithLocation Import]
  -> m Scope
createImportedScope moduleInterfaces imports = do
  -- for each imported module, we create a hashmap from module name to scope:
  -- the hashmap of imported paths, grouped by module
  knownSymbols :: [HashMap ModuleName Scope] <-
    for imports \importWithLocation -> do
      let
        importLocation = _location importWithLocation
        Import {..} = _located importWithLocation
        mkDiagnostic = Diagnostic Nothing importLocation
      exportedIdentifiers <-
        fmap _interfaceExported $
          M.lookup _importPath moduleInterfaces `onNothing`
            reportError (mkDiagnostic $ ErrorImportPath _importPath)
      M.singleton _importPath . M.fromListWith (<>) <$> case _importType of
        Qualified Nothing ->
          pure $ M.toList exportedIdentifiers <&> \(identifier, role) ->
            (_importPath <> pure identifier, pure role)
        Qualified (Just qualifier) ->
          pure $ M.toList exportedIdentifiers >>= \(identifier, role) ->
            [ (pure qualifier <> pure identifier, pure role)
            , (_importPath <> pure identifier, pure role)
            ]
        Exhaustive ->
          pure $ M.toList exportedIdentifiers >>= \(identifier, role) ->
            [ (pure identifier, pure role)
            , (_importPath <> pure identifier, pure role)
            ]
        Specific identifiers ->
          concat <$> for identifiers \identifier -> do
            role <- M.lookup identifier exportedIdentifiers `onNothing`
              reportError (mkDiagnostic $ ErrorImportSymbol _importPath identifier)
            pure
              [ (pure identifier, pure role)
              , (_importPath <> pure identifier, pure role)
              ]

  -- we group the declarations per module, using (<>) on the hashmap:
  -- this discards duplicates within the same module, as the same
  -- module might appear more than once in the list of imports.
  -- we then concatenate the non-empty lists, which confusingly is also
  -- a union with (<>), but on the non-empty lists.
  -- the result is a hashmap from path to grouped non-empty list of
  -- possible matches across modules
  pure $
    foldl' (M.unionWith (<>)) M.empty $ M.elems $
    foldl' (M.unionWith (<>)) M.empty knownSymbols

parseLocalDeclarations
  :: MonadDiagnosis m
  => ModuleName
  -> [WithLocation Parsed.Definition]
  -> m
     ( HashMap Identifier Role
     , [(BaseName, WithLocation Parsed.Definition)]
     , Scope
     )
parseLocalDeclarations moduleName definitions = do
  let
    topLevelBindings :: HashMap Identifier (NonEmpty (Role, WithLocation Parsed.Definition))
    topLevelBindings = M.fromListWith (<>) do
      definition <- definitions
      (identifier, role) <- declarationTopLevelBindings moduleName $ _located definition
      pure (identifier, pure (role, definition))

  validatedBindings :: HashMap Identifier (Role, WithLocation Parsed.Definition) <-
    ensureNested $
      M.forWithKey topLevelBindings \identifier entries -> tryNested do
        let
          locations = _location . snd <$> entries
          firstLocation = NE.head locations
        when (NE.length locations > 1) $
          reportError $ Diagnostic Nothing firstLocation $ ErrorMultipleDeclaration identifier locations
        when (isReserved identifier) $
          reportError $ Diagnostic Nothing firstLocation $ ErrorReservedIdentifier identifier
        pure $ NE.head entries

  let
    exports :: HashMap Identifier Role
    exports = fst <$> validatedBindings

    definitionList :: [(BaseName, WithLocation Parsed.Definition)]
    definitionList = do
      definition <- definitions
      (identifier, _) <- declarationTopLevelBindings moduleName $ _located definition
      let baseName = BaseName moduleName identifier
      pure (baseName, definition)

    scope :: Scope
    scope = M.fromListWith (<>) do
      (identifier, (role, _)) <- M.toList validatedBindings
      path <- [pure identifier, moduleName <> pure identifier]
      pure (path, pure role)

  pure (exports, definitionList, scope)

declarationTopLevelBindings
  :: ModuleName
  -> Parsed.Definition
  -> [(Identifier, Role)]
declarationTopLevelBindings moduleName = \case
  TypeAliasDef info -> [createBinding TypeAlias $ _aliasName  info]
  StructDef    info -> [createBinding Struct    $ _structName info]
  ConstDef     info -> [createBinding Constant  $ _constName  info]
  FunctionDef  info -> [createBinding Function  $ _funName    info]
  EnumDef      info ->
    createBinding Enum (_enumName info) :
      map (createBinding Constant) (_enumValues info)
  where
    createBinding cons identifier =
      (identifier, cons $ BaseName moduleName identifier)


--------------------------------------------------------------------------------
-- Scope manipulation

expandScopeWithTypeParameters
  :: [Identifier]
  -> Resolve ()
expandScopeWithTypeParameters parameters = do
  declName <- view riDeclarationName
  ensureNested $ traverse
    (tryNested . fatal . ErrorDuplicatedTypeParameter)
    (findDuplicates parameters)
  bindings <- ensureNested $
    for parameters \identifier -> tryNested do
      let role = TypeParameter declName identifier
      validateBinding identifier role
      pure (pure identifier, pure role)
  rcScope %= M.union (M.fromList bindings)

expandScopeWithFunctionArguments
  :: [(Identifier, Resolved.FunctionArgType)]
  -> Resolve ()
expandScopeWithFunctionArguments arguments = do
  ensureNested $ traverse
    (tryNested . fatal . ErrorFunctionDuplicatedArg)
    (findDuplicates $ map fst arguments)
  bindings <- ensureNested $
    for arguments \(argName, argType) -> tryNested do
      let role = FunctionArgument argName argType
      validateBinding argName role
      pure (pure argName, pure role)
  rcScope %= M.union (M.fromList bindings)

expandScopeWithVariable
  :: Identifier
  -> Resolve ()
expandScopeWithVariable varName = do
  let role = LetVariable varName
  validateBinding varName role
  rcScope %= M.union (M.singleton (pure varName) (pure role))
