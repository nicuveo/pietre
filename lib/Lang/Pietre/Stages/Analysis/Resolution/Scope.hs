module Lang.Pietre.Stages.Analysis.Resolution.Scope where

import "this" Prelude

import Control.Lens                                 hiding (mapping, op)
import Control.Monad.Loops                          (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                    (hoistMaybe)
import Data.HashMap.Strict.Extra                    qualified as M
import Data.HashSet                                 qualified as S
import Data.Set                                     qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Parsed       as Parsed
import Lang.Pietre.Representations.AST.Resolved     as Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Resolution.Monad


--------------------------------------------------------------------------------
-- Scope creation

createImportedScope
  :: MonadDiagnostic m
  => HashMap ModuleName Interface
  -> [Import]
  -> m Scope
createImportedScope moduleInterfaces imports = do
  -- for each imported module, we create a hashmap from module name to scope:
  -- the hashmap of imported paths, grouped by module
  knownSymbols :: [HashMap ModuleName Scope] <-
    for imports \Import {..} -> do
      exportedIdentifiers <-
        fmap _interfaceExported $
          M.lookup _importPath moduleExports `onNothing`
            report (ErrorImportPath _importPath)
      M.singleton _importPath . M.fromListWith (<>) <$> case _importType of
        Qualified Nothing ->
          pure $ M.toList exportedIdentifiers <&> \(identifier, role) ->
            (_importPath <> pure identifier, role)
        Qualified (Just qualifier) ->
          pure $ M.toList exportedIdentifiers >>= \(identifier, role) ->
            [ (pure qualifier <> pure identifier, role)
            , (_importPath <> pure identifier, role)
            ]
        Exhaustive ->
          pure $ M.toList exportedIdentifiers >>= \(identifier, role) ->
            [ (pure identifier, role)
            , (_importPath <> pure identifier, role)
            ]
        Specific identifiers ->
          concat <$> for identifiers \identifier -> do
            role <- M.lookup identifier exportedIdentifiers `onNothing`
              report $ ErrorImportSymbol _importPath identifier
            pure
              [ (pure identifier, role)
              , (_importPath <> pure identifier, role)
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
  :: MonadDiagnostic m
  => ModuleName
  -> [WithLocation Parsed.Definition]
  -> m
     ( HashMap Identifier Role
     , HashMap BaseName (WithLocation Parsed.Definition)
     , Scope
     )
parseLocalDeclarations moduleName definitions = do
  let topLevelBindings :: HashMap Identifier (NonEmpty (Role, Definition))
      topLevelBindings = M.fromList (<>) do
        definition <- definitions
        (identifier, role) <- declarationTopLevelBindings moduleName $ _located definition
        pure (identifier, pure (role, definition))

  validatedBindings :: HashMap Identifier (Role, Definition)
  validatedBindings <-
    ensureNested $
      flip M.traverseWithKey topLevelBindings \(identifier, entries) -> tryNested do
        let locations = _location . snd <$> entries
        when (NE.length locations > 1) $
          report $ ErrorMultipleDeclaration identifier locations
        when (isReserved identifier) $
          report $ ErrorReservedIdentifier identifier
        pure $ NE.head entries

  let exports :: HashMap Identifier Role
      exports = fst <$> validatedBindings

  let definitions :: HashMap BaseName Definition
      definitions = M.fromList do
        (identifier, (_, definition)) <- M.toList validatedBindings
        let baseName = BaseName moduleName identifier
        pure (baseName, definition)

  let scope :: Scope
      scope = M.fromListWith (<>) do
        (identifier, (role, _)) <- M.toList validatedBindings
        path <- [pure identifier, moduleName <> pure identifier]
        pure (path, pure role)

  pure (exports, definitions, scope)

declarationTopLevelBindings
  :: ModuleName
  -> Parsed.Declaration
  -> [(Identifier, Role)]
declarationTopLevelBindings = \case
  TypeAliasDef info -> [createBinding TypeAlias $ _aliasName  info]
  StructDef    info -> [createBinding Struct    $ _structName info]
  ConstDef     info -> [createBinding Constant  $ _constName  info]
  FunctionDef  info -> [createBinding Function  $ _funName    info]
  EnumDef      info ->
    createBinding Enum (_enumName info) :
      map (createBinding Constant) _enumValues
  where
    createBinding cons identifier =
      (identifier, cons $ BaseName moduleName identifier)


--------------------------------------------------------------------------------
-- Scope manipulation

expandScopeWithTypeParameters
  :: Monad m
  => [Identifier]
  -> ResolveT m ()
expandScopeWithTypeParameters parameters = do
  declName <- use riName
  ensure . sequence =<< traverse
    (try . fatal . ErrorDuplicatedTypeParameter)
    (findDuplicates parameters)
  bindings <- ensure . sequence =<<
    for parameters \identifier -> do
      let role = TypeParameter declName identifier
      result <- try $ validateBinding identifier role
      (pure identifier, pure role) <$ result
  rcScope %= M.union (M.fromList bindings)

expandScopeWithFunctionArguments
  :: Monad m
  => [(Identifier, FunctionArgType Resolved)]
  -> ResolveT m ()
expandScopeWithFunctionArguments arguments = do
  ensure . sequence =<< traverse
    (try . fatal . ErrorFunctionDuplicatedArg)
    (findDuplicates $ map fst arguments)
  bindings <- ensure . sequence =<<
    for arguments \(argName, argType) -> do
      let role = FunctionArgType argName argType
      result <- try $ validateBinding argName role
      (pure identifier, pure role) <$ result
  rcScope %= M.union (M.fromList bindings)

expandScopeWithVariable
  :: Monad m
  => Identifier
  -> Maybe (PathInfo Resolved)
  -> ResolveT m ()
expandScopeWithVariable varName varType = do
  let role = LetVariable varName varType
  validateBinding varName role
  rcScope %= M.union (M.singleton varName role)
