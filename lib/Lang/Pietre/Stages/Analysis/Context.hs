module Lang.Pietre.Stages.Analysis.Context
  ( -- * lookup
    lookupRole
  , lookupEnumType
    -- * type resolve
  , TypeResolutionMode (..)
  , resolveType
  , resolveStructType
  , checkStructType
    -- * expr resolve
  , resolveConstValue
  , resolveExprValue
  , resolveFunctionCallValue
  ) where

import                "this" Prelude

import                Control.Lens                              hiding (mapping,
                                                                 op)
import                Data.HashMap.Strict                       qualified as M
import                Data.List                                 qualified as L
import                Data.List.NonEmpty                        qualified as NE

import                Lang.Pietre.Batteries.BuiltIn
import                Lang.Pietre.Representations.AST
import                Lang.Pietre.Representations.Location
import                Lang.Pietre.Representations.Name
import                Lang.Pietre.Representations.Tokens
import {-# SOURCE #-} Lang.Pietre.Stages.Analysis.Core
import                Lang.Pietre.Stages.Analysis.Diagnostic
import                Lang.Pietre.Stages.Analysis.Instantiation
import                Lang.Pietre.Stages.Analysis.Monad


--------------------------------------------------------------------------------
-- Lookup

-- | Given an identifier, attempt to locate any matching item in scope.
lookupRole
  :: Identifier
  -> AnalysisM (Maybe (NonEmpty Role))
lookupRole path =
  uses contextScope (M.lookup $ pure path)

lookupEnumType
  :: PathInfo Resolved
  -> AnalysisM (Maybe (EnumInfo Resolved))
lookupEnumType = resolveTypeWith go
  where
    go :: ResolveCallback (Maybe (EnumInfo Resolved))
    go _ info = sequence do
      WithLocation loc (EnumDef enumInfo) <- fmap snd info
      pure $ forceDefinition loc enumInfo


--------------------------------------------------------------------------------
-- Type-level resolve

data TypeResolutionMode
  = AllowPlaceholder
  | ForbidPlaceholder Text
  deriving (Show)

resolveType
  :: TypeResolutionMode
  -> PathInfo Parsed
  -> AnalysisM (PathInfo Resolved)
resolveType mode =
  resolvePath mode >=> resolveTypeWith go
  where
    go :: ResolveCallback (PathInfo Resolved)
    go path@PathInfo {..} = \case
      Nothing -> case (_pathName, mode) of
        (Placeholder, ForbidPlaceholder context) ->
          fatal $ ErrorPlaceholder context
        _ -> pure path
      Just (name, def) -> case _located def of
        ConstDef     _ -> fatal $ ErrorNotAType _pathName
        FunctionDef  _ -> fatal $ ErrorNotAType _pathName
        TypeAliasDef info -> do
          resolvedInfo <- forceDefinition (_location def) info
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            report $ ErrorIncorrectTypeParameterCount name expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          substituteTypes typeArguments $ _aliasValue resolvedInfo
        EnumDef enumInfo -> do
          let actual = length _pathParams
          unless (null _pathParams) $
            report $ ErrorIncorrectTypeParameterCount name 0 actual
          let identifier = NE.last $ _nameFullPath name
          when (identifier /= _enumName enumInfo) $
            report $ ErrorNotAType _pathName
          pure path
        StructDef info -> do
          let expected = length (_structParams info)
              actual   = length _pathParams
              invalid  = case mode of
                AllowPlaceholder    -> actual > 0 && actual /= expected
                ForbidPlaceholder _ -> actual /= expected
          when invalid $
            report $ ErrorIncorrectTypeParameterCount name expected actual
          pure path

resolveStructType
  :: PathInfo Parsed
  -> AnalysisM
     (Maybe ( PathInfo Resolved
            , StructInfo Resolved
            , HashMap Identifier (PathInfo Resolved)
            )
     )
resolveStructType path = do
  resolvedPath <- resolveType AllowPlaceholder path
  result <- checkStructType resolvedPath
  pure $ result <&> \(info, mappings) -> (resolvedPath, info, mappings)

checkStructType
  :: PathInfo Resolved
  -> AnalysisM
     (Maybe ( StructInfo Resolved
            , HashMap Identifier (PathInfo Resolved)
            )
     )
checkStructType = resolveTypeWith go
  where
    go :: ResolveCallback
          (Maybe ( StructInfo Resolved
                 , HashMap Identifier (PathInfo Resolved)
                 )
          )
    go PathInfo {..} = \case
      Nothing ->
        case _pathName of
          TypeParameter _ ->
            pure Nothing
          Placeholder ->
            fatal $ ErrorPlaceholder "struct name in struct expression"
          _ ->
            fatal $ ErrorNotAStruct _pathName
      Just (_, def) -> case _located def of
        EnumDef      _ -> fatal $ ErrorNotAStruct _pathName
        StructDef info -> do
          resolvedInfo <- forceDefinition (_location def) info
          let expectedParams = _structParams resolvedInfo
              mapping = M.fromList $
                if null _pathParams
                then [(paramName, PlaceholderType) | paramName <- expectedParams]
                else zip expectedParams _pathParams
          pure $ Just (resolvedInfo, mapping)
        _ -> error "ICE"


--------------------------------------------------------------------------------
-- Value-level resolve

resolveConstValue
  :: PathInfo Parsed
  -> AnalysisM TypedExpression
resolveConstValue = do
  resolvePath (ForbidPlaceholder "const value") >=> resolveValueWith go
  where
    go :: ResolveCallback TypedExpression
    go resolvedPath@PathInfo {..} = \case
      Nothing -> error "ICE"
      Just (name, def) -> case _located def of
        TypeAliasDef _ -> fatal $ ErrorNotAConst _pathName
        FunctionDef  _ -> fatal $ ErrorNotAConst _pathName
        StructDef    _ -> fatal $ ErrorNotAConst _pathName
        EnumDef   info -> verifyEnumValue resolvedPath name info
        ConstDef  info -> verifyConstValue resolvedPath name (_location def) info

resolveExprValue
  :: PathInfo Parsed
  -> AnalysisM TypedExpression
resolveExprValue =
  resolvePath mode >=> resolveValueWith go
  where
    mode = ForbidPlaceholder "expr value"
    go :: ResolveCallback TypedExpression
    go resolvedPath@PathInfo {..} info = case _pathName of
      FunctionArgument _ (ByValue argType) ->
        pure $ TypedExpression argType $ PathExpr resolvedPath
      FunctionArgument _ (ByReference argType) ->
        pure $ TypedExpression argType $ PathExpr resolvedPath
      LetVariable _ varType ->
        pure $ TypedExpression varType $ PathExpr resolvedPath
      BuiltinFunction _ -> case info of
        Just (name, WithLocation _ (FunctionDef funInfo)) -> do
          (_, functionType, _) <- partiallyResolveFunctionType mode resolvedPath name funInfo
          pure $ TypedExpression
            (PathInfo (FunctionPointer functionType) [])
            (PathExpr resolvedPath)
        _ -> error "ICE"
      TopLevelDeclaration _ -> case info of
        Nothing -> error "ICE"
        Just (name, def) -> case _located def of
          TypeAliasDef    _ -> fatal $ ErrorNotAValue _pathName
          StructDef       _ -> fatal $ ErrorNotAValue _pathName
          EnumDef     eInfo -> verifyEnumValue resolvedPath name eInfo
          ConstDef    cInfo -> verifyConstValue resolvedPath name (_location def) cInfo
          FunctionDef fInfo -> do
            (_, functionType, _) <- partiallyResolveFunctionType mode resolvedPath name fInfo
            pure $ TypedExpression
              (PathInfo (FunctionPointer functionType) [])
              (PathExpr resolvedPath)
      _ -> error "ICE"

resolveFunctionCallValue
  :: PathInfo Parsed
  -> AnalysisM
     ( PathInfo Resolved
     , FunctionType Resolved
     , HashMap Identifier (PathInfo Resolved)
     )
resolveFunctionCallValue = do
  resolvePath AllowPlaceholder >=> resolveValueWith go
  where
    go :: ResolveCallback
          ( PathInfo Resolved
          , FunctionType Resolved
          , HashMap Identifier (PathInfo Resolved)
          )
    go resolvedPath@PathInfo {..} info = case _pathName of
      FunctionArgument _ (ByValue argType) ->
        checkFunctionType resolvedPath argType
      FunctionArgument _ (ByReference argType) ->
        checkFunctionType resolvedPath argType
      LetVariable _ varType ->
        checkFunctionType resolvedPath varType
      BuiltinFunction _ -> case info of
        Just (name, WithLocation _ (FunctionDef fInfo)) -> do
          partiallyResolveFunctionType AllowPlaceholder resolvedPath name fInfo
        _ -> error "ICE"
      TopLevelDeclaration _ -> case info of
        Nothing -> error "ICE"
        Just (name, def) -> case _located def of
          TypeAliasDef    _ -> fatal $ ErrorNotAFunction _pathName
          StructDef       _ -> fatal $ ErrorNotAFunction _pathName
          EnumDef         _ -> fatal $ ErrorNotAFunction _pathName
          ConstDef        _ -> fatal $ ErrorNotAFunction _pathName
          FunctionDef fInfo -> partiallyResolveFunctionType AllowPlaceholder resolvedPath name fInfo
      _ -> error "ICE"
    checkFunctionType resultPath PathInfo {..} = case _pathName of
      FunctionPointer functionType ->
        pure (resultPath, functionType, M.empty)
      _ ->
        fatal $ ErrorNotAFunction _pathName


--------------------------------------------------------------------------------
-- Analyzable class

class Analyzable p where
  forceDefinition
    :: IsDefinition i
    => Location
    -> i p
    -> AnalysisM (i Resolved)
  forceType
    :: TypeResolutionMode
    -> PathInfo p
    -> AnalysisM (PathInfo Resolved)

instance Analyzable Parsed where
  forceDefinition loc info = toDefinition info
    & WithLocation loc
    & analyzeDefinition
    & fmap (fromMaybe (error "ICE") . fromDefinition)
  forceType = resolveType

instance Analyzable Resolved where
  forceDefinition _ = pure
  forceType _ = pure

class IsDefinition i where
  toDefinition :: i p -> Definition p
  fromDefinition :: Definition p -> Maybe (i p)

instance IsDefinition TypeAliasInfo where
  toDefinition = TypeAliasDef
  fromDefinition = \case
    TypeAliasDef i -> Just i
    _ -> Nothing

instance IsDefinition EnumInfo where
  toDefinition = EnumDef
  fromDefinition = \case
    EnumDef i -> Just i
    _ -> Nothing

instance IsDefinition StructInfo where
  toDefinition = StructDef
  fromDefinition = \case
    StructDef i -> Just i
    _ -> Nothing

instance IsDefinition ConstInfo where
  toDefinition = ConstDef
  fromDefinition = \case
    ConstDef i -> Just i
    _ -> Nothing

instance IsDefinition FunctionInfo where
  toDefinition = FunctionDef
  fromDefinition = \case
    FunctionDef i -> Just i
    _ -> Nothing


--------------------------------------------------------------------------------
-- Internals

type ResolveCallback r
  =  forall (p :: ASTPhase)
  .  Analyzable p
  => PathInfo Resolved
  -> Maybe (Name, WithLocation (Definition p))
  -> AnalysisM r

resolvePath
  :: TypeResolutionMode
  -> PathInfo Parsed
  -> AnalysisM (PathInfo Resolved)
resolvePath mode PathInfo {..} = do
  roles@(role :| others) <-
    uses contextScope (M.lookup _pathName) `onNothingM`
      fatal (ErrorRoleNotFound _pathName)
  unless (null others) $
    fatal $ ErrorAmbiguousPath _pathName roles
  params <- traverse (resolveType mode) _pathParams
  pure $ PathInfo role params

resolveTypeWith
  :: forall r
   . ResolveCallback r
  -> PathInfo Resolved
  -> AnalysisM r
resolveTypeWith callback resolvedPathInfo = do
  name <- getTypeName (_pathName resolvedPathInfo)
  ensure $ processCallback callback resolvedPathInfo name

resolveValueWith
  :: forall r
   . ResolveCallback r
  -> PathInfo Resolved
  -> AnalysisM r
resolveValueWith callback resolvedPathInfo = do
  name <- getValueName (_pathName resolvedPathInfo)
  ensure $ processCallback callback resolvedPathInfo name

getTypeName :: Role -> AnalysisM (Maybe Name)
getTypeName role = case role of
  TopLevelDeclaration name -> pure $ Just name
  BuiltinType _            -> pure Nothing
  BuiltinFunction _        -> fatal $ ErrorNotAType role
  TypeParameter _          -> pure Nothing
  Placeholder              -> pure Nothing
  FunctionPointer _        -> pure Nothing
  FunctionArgument _ _     -> fatal $ ErrorNotAType role
  LetVariable _ _          -> fatal $ ErrorNotAType role

getValueName :: Role -> AnalysisM (Maybe Name)
getValueName role = case role of
  TopLevelDeclaration name   -> pure  $ Just name
  BuiltinType _              -> fatal $ ErrorNotAValue role
  BuiltinFunction name       -> pure  $ Just name
  TypeParameter _            -> fatal $ ErrorNotAValue role
  Placeholder                -> fatal $ ErrorNotAValue role
  FunctionPointer _          -> fatal $ ErrorNotAValue role
  FunctionArgument _ argType -> getTypeName $ _pathName $ functionArgType argType
  LetVariable _ varType      -> getTypeName $ _pathName varType

processCallback
  :: forall r
   . ResolveCallback r
  -> PathInfo Resolved
  -> Maybe Name
  -> AnalysisM r
processCallback callback resolvedPathInfo = \case
  Nothing -> callback @Resolved resolvedPathInfo Nothing
  Just name -> do
    let
      call :: forall (p :: ASTPhase)
           .  Analyzable p
           => WithLocation (Definition p)
           -> AnalysisM r
      call = callback resolvedPathInfo . Just . (name,)
    foreignDefinition <- views infoForeignDefinitions (M.lookup name)
    cachedDefinition  <- uses contextCache (M.lookup name)
    localDefinition   <- views infoLocalDefinitions (M.lookup name)
    let action = asum [ call <$> foreignDefinition
                      , call <$> cachedDefinition
                      , call <$> localDefinition
                      ]
    fromMaybe (error "ICE") action

partiallyResolveFunctionType
  :: forall (p :: ASTPhase)
   . Analyzable p
  => TypeResolutionMode
  -> PathInfo Resolved
  -> Name
  -> FunctionInfo p
  -> AnalysisM
     ( PathInfo Resolved
     , FunctionType Resolved
     , HashMap Identifier (PathInfo Resolved)
     )
partiallyResolveFunctionType mode resolvedPath@PathInfo {..} name FunctionInfo {..} = do
  resolvedArgs   <- traverse (forceFunArg . snd) _funArgs
  resolvedReturn <- traverse forceFunType _funReturn
  let expected = length _funParams
      actual   = length _pathParams
      invalid  = case mode of
        AllowPlaceholder    -> actual > 0 && actual /= expected
        ForbidPlaceholder _ -> actual /= expected
  when invalid $
    report $ ErrorIncorrectTypeParameterCount name expected actual
  let mapping = M.fromList $
        if null _pathParams
        then [(paramName, PlaceholderType) | paramName <- _funParams]
        else zip _funParams _pathParams
  -- TODO: register function for instantiation
  pure (resolvedPath, FunctionType _funParams resolvedArgs resolvedReturn, mapping)
  where
    forceFunType = forceType @p $ ForbidPlaceholder "function call"
    forceFunArg = \case
      ByValue     path -> ByValue     <$> forceFunType path
      ByReference path -> ByReference <$> forceFunType path

verifyEnumValue
  :: forall p
   . PathInfo Resolved
  -> Name
  -> EnumInfo p
  -> AnalysisM TypedExpression
verifyEnumValue resolvedPath@PathInfo {..} name EnumInfo {..} = do
  let actual = length _pathParams
  when (actual /= 0) $
    report $ ErrorIncorrectTypeParameterCount name 0 actual
  let identifier = NE.last $ _nameFullPath name
  when (identifier == _enumName) $
    report $ ErrorNotAConst _pathName
  case L.elemIndex identifier _enumValues of
    Nothing -> error "ICE"
    Just i  -> pure $ TypedExpression resolvedPath $ IntLiteralExpr i

verifyConstValue
  :: forall p
   . Analyzable p
  => PathInfo Resolved
  -> Name
  -> Location
  -> ConstInfo p
  -> AnalysisM TypedExpression
verifyConstValue PathInfo {..} name loc constInfo = do
  let actual = length _pathParams
  when (actual /= 0) $
    report $ ErrorIncorrectTypeParameterCount name 0 actual
  _constExpr <$> forceDefinition loc constInfo
