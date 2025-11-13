module Lang.Pietre.Stages.Analysis.Validation.Types where

import "this" Prelude


validateConcreteType
  :: Monad m
  => PathInfo Resolved
  -> Validate m Type
validateConcreteType = go M.empty
  where
    go localMappings PathInfo {..} = do
      params <- traverse (go localMappings) _pathParams
      case _pathName of
        BuiltinType name                 -> validateBuiltinType name params
        Struct baseName                  -> validateStructType baseName params
        Enum baseName                    -> validateEnumType baseName params
        TypeAlias baseName               -> validateTypeAliasType go localMappings baseName params
        TypeParameter typeName paramName -> validateTypeParameterType localMappings typeName paramName
        FunctionPointer functionType     -> unimplemented
        BuiltinFunction _                -> fatal $ ErrorNotAType _pathName
        Constant baseName                -> fatal $ ErrorNotAType _pathName
        Function baseName                -> fatal $ ErrorNotAType _pathName
        Placeholder                      -> fatal $ ErrorNotAType _pathName
        FunctionArgument argName argType -> fatal $ ErrorNotAType _pathName
        LetVariable varName varType      -> fatal $ ErrorNotAType _pathName

    validateStructType baseName actualParams = do
      expectedParams <- retrieveStructParams baseName
      validateParamsCount expectedParams actualParams
      pure $ StructTypeInfo
        { _structName       = baseName
        , _structTypeParams = actualParams
        }

validatePartialType
  :: Monad m
  => PathInfo Resolved
  -> Validate m PartialType
validatePartialType path =
  go M.empty path `onNothingM`
    report $ ErrorPlaceholder "placeholder at root"
  where
    go localMappings PathInfo {..} = do
      params <- traverse (go localMappings) _pathParams
      case _pathName of
        BuiltinType name                 -> Just <$> validateBuiltinType name params
        Struct baseName                  -> validateStructType baseName params
        Enum baseName                    -> validateEnumType baseName params
        TypeAlias baseName               -> validateTypeAliasType go localMappings baseName params
        TypeParameter typeName paramName -> validateTypeParameterType localMappings typeName paramName
        FunctionPointer functionType     -> unimplemented
        Placeholder                      -> pure Nothing
        BuiltinFunction _                -> fatal $ ErrorNotAType _pathName
        Constant baseName                -> fatal $ ErrorNotAType _pathName
        Function baseName                -> fatal $ ErrorNotAType _pathName
        FunctionArgument argName argType -> fatal $ ErrorNotAType _pathName
        LetVariable varName varType      -> fatal $ ErrorNotAType _pathName

    validateStructType baseName actualParams = do
      expectedParams <- retrieveStructParams baseName
      validateParamsCountWith (<=) expectedParams actualParams
      pure $ StructTypeInfo
        { _structName       = baseName
        , _structTypeParams = actualParams
        }

validateParameterizedType
  :: Monad m
  => PathInfo Resolved
  -> Validate m ParameterizedType
validateParameterizedType = go M.empty
  where
    go localMappings PathInfo {..} = do
      params <- traverse (go localMappings) _pathParams
      case _pathName of
        BuiltinType name                 -> Right <$> validateBuiltinType name params
        Struct baseName                  -> validateStructType baseName params
        Enum baseName                    -> validateEnumType baseName params
        TypeAlias baseName               -> validateTypeAliasType go localMappings baseName params
        TypeParameter typeName paramName -> validateTypeParameterType localMappings typeName paramName
        FunctionPointer functionType     -> unimplemented
        Placeholder                      -> report $ ErrorPlaceholder unimplemented
        BuiltinFunction _                -> report $ ErrorNotAType _pathName
        Constant baseName                -> report $ ErrorNotAType _pathName
        Function baseName                -> report $ ErrorNotAType _pathName
        FunctionArgument argName argType -> report $ ErrorNotAType _pathName
        LetVariable varName varType      -> report $ ErrorNotAType _pathName
    validateStructType baseName actualParams = do
      expectedParams <- retrieveStructParams baseName
      validateParamsCount expectedParams actualParams
      pure $ StructTypeInfo
        { _structName       = baseName
        , _structTypeParams = actualParams
        }

validateBuiltinType
  :: Monad m
  => Name
  -> [TypeTree f]
  -> Validate m (TypeNode f)
validateBuiltinType name params = case name of
  IntName  -> validateNoParams *> IntType
  CharName -> validateNoParams *> CharType
  BoolName -> validateNoParams *> BoolType
  UnitName -> validateNoParams *> UnitType
  VoidName -> pure VoidType
  where
    validateNoParams =
      when (length params > 0) $
        report $ ErrorIncorrectTypeParameterCount name 0 (length params)

validateEnumType
  :: Monad m
  => BaseName
  -> [TypeTree f]
  -> Validate m (TypeNode f)
validateEnumType baseName params = do
  validateParamsCount [] actualParams
  values <- retrieveEnum baseName
  pure $ EnumType baseName values

validateTypeAliasType
  :: Monad m
  => (    M.HashMap (BaseName, Identifier) t
       -> PathInfo Resolved
       -> Validate m t
     )
  -> M.HashMap (BaseName, Identifier) t
  -> BaseName
  -> [t]
  -> Validate m t
validateTypeAliasType f localMappings baseName params = do
  TypeAliasInfo {..} <- retrieveTypeAlias baseName
  validateParamsCount _aliasParams params
  let newMappings = M.fromList $ zip (map (baseName,) _aliasParams) params
  f (M.union newMappings localMappings) _aliasValue

validateTypeAliasType
  :: (Applicative f, Monad m)
  => M.HashMap (BaseName, Identifier) (TypeTree f)
  -> BaseName
  -> [TypeTree f]
  -> Validate m (TypeTree f)
validateTypeAliasType localMappings typeName paramName =
  M.lookup (typeName, paramName) localMappings `onNothing`
    fmap (abstract pure) (retrieveTypeParameter typeName paramName)

buildTypeParameterMap
  :: Monad m
  => ParameterizedType
  -> Type
  -> Validate m [(Identifier, Type)]
buildTypeParameterMap expected actual =
  go expected actual
  where
    abort = report (ErrorIncompatibleType expected actual)
    go e a = case (e, a) of
      (Left identifier, _) ->
        pure [(identifier, a)]
      (Right (StructType structType), VoidType) -> do
        concat <$> zipWithM go (snd <$> _structFields structType) (VoidType <$ _structFields structType)
      (_, VoidType) ->
        pure []
      (Right IntType, IntType) ->
        pure []
      (Right BoolType, BoolType) ->
        pure []
      (Right CharType, CharType) ->
        pure []
      (Right UnitType, UnitType) ->
        pure []
      (Right (EnumType name1 _), EnumType name2 _) -> do
        unless (name1 == name2) abort
        pure []
      (Right (StructType structType1), StructType structType2) -> do
        unless (_structName structType1 == _structName structType2) abort
        concat <$> zipWithM go (snd <$> _structFields structType1) (snd <$> _structFields structType2)
      _ ->
        abort

validateTypePattern
  :: Monad m
  => PartialType
  -> Type
  -> Validate m ()
validateTypePattern expected actual =
  go expected actual
  where
    abort = report (ErrorIncompatibleType expected actual)
    go e a = case (e, a) of
      (Nothing, _) -> pass
      (_, VoidType) -> pass
      (Just IntType, IntType) -> pass
      (Just BoolType, BoolType) -> pass
      (Just CharType, CharType) -> pass
      (Just UnitType, UnitType) -> pass
      (Just (EnumType name1 _), EnumType name2 _) -> do
        unless (name1 == name2) abort
      (Just (StructType structType1), StructType structType2) -> do
        unless (_structName structType1 == _structName structType2) abort
      _ ->
        abort

validateParamsCount
  :: Monad m
  => [Identifier]
  -> [t]
  -> Validate m ()
validateParamsCount = validateParamsCountWith (==)

validateParamsCountWith
  :: Monad m
  => (Int -> Int -> Bool)
  -> [Identifier]
  -> [t]
  -> Validate m ()
validateParamsCountWith cmp expectedParams actualParams = do
  let actual   = length actualParams
      expected = length expectedParams
  unless (actual `cmp` expected) $
    report $ ErrorIncorrectTypeParameterCount baseName expected actual

reifyType
  :: Applicative f
  => HashMap Identifier (TypeTree f)
  -> ParameterizedType
  -> TypeTree f
reifyType mappings = \case
  Left identifier ->
    lookupParameter identifier
  Right IntType ->
    hpure IntType
  Right BoolType ->
    hpure BoolType
  Right CharType ->
    hpure CharType
  Right UnitType ->
    hpure UnitType
  Right VoidType ->
    hpure VoidType
  Right (EnumType name values) ->
    hpure $ EnumType name values
  Right (StructType structInfo) ->
    hpure $ StructType $ reifyStructType structInfo
  Right (FunctionType functionInfo) ->
    hpure $ FunctionType $ reifyFunctionType functionInfo
  where
    reifyFunctionType FunctionTypeInfo {..} = FunctionTypeInfo
      { _funArgs   = fmap reifyFunctionArg _funArgs
      , _funReturn = fmap (reifyType mappings) _funReturn
      }

    reifyFunctionArg = \case
      ByValue     t -> ByValue     $ reifyType mappings t
      ByReference t -> ByReference $ reifyType mappings t

    reifyStructType StructTypeInfo {..} = StructTypeInfo
      { _structName       = _structName
      , _structTypeParams = fmap (reifyType mappings) _structTypeParams
      , _structFields     = fmap2 (reifyType mappings) _structFields
      , _structInfo       = _structInfo
      }

    lookupParameter identifier =
      M.lookup identifier mappins `onNothing`
        reportICE
          "type reification"
          "no information for type parameter"
          [ "parameter name:   " ++ show name
          , "known parameters: " ++ show mappings
          ]

typeMatches
  :: Type
  -> Type
  -> Bool
typeMatches a b =
  case (a,b) of
    (VoidType,        _)               -> True
    (_,               VoidType)        -> True
    (IntType,         IntType)         -> True
    (BoolType,        BoolType)        -> True
    (CharType,        CharType)        -> True
    (UnitType,        UnitType)        -> True
    (EnumType n1 _,   EnumType n2 _)   -> n1 == n2
    (StructType s1,   StructType s2)   -> structsMatch s1 s2
    (FunctionType f1, FunctionType f2) -> f1 == f2
    _                                  -> False
  where
    structsMatch s1 s2 =
      (_structBaseName s1 == _structBaseName s2) &&
      and (zipWith typeMatches (_structTypeParams s1) (_structTypeParams s2))

typesAllMatch
  :: [Type]
  -> Bool
typesAllMatch types = and do
  (headType : remainingTypes) <- L.tails types
  otherType <- remainingTypes
  pure $ headType `typeMatches` otherType
