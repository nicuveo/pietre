{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Stages.Analysis.Validation.Types where

import "this" Prelude

import Control.Lens                                   hiding (mapping, op)
import Data.Either.Extra                              (eitherToMaybe)
import Data.HashMap.Strict.Extra                      qualified as M

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.HKT
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved       as Resolved hiding
                                                                  (pattern FunctionType)
import Lang.Pietre.Representations.AST.Validated      as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Context
import Lang.Pietre.Stages.Analysis.Validation.Monad


validateConcreteType
  :: Resolved.PathInfo
  -> Validate ConcreteType
validateConcreteType = go M.empty
  where
    go
      :: HashMap Identifier ConcreteType
      -> Resolved.PathInfo
      -> Validate ConcreteType
    go localMappings PathInfo {..} = do
      params <- traverse (go localMappings) _pathParams
      case _pathBase of
        BuiltinType name                 -> validateBuiltinType name params
        Struct baseName                  -> validateStructType baseName params
        Enum baseName                    -> validateEnumType baseName params
        TypeAlias baseName               -> validateTypeAliasType @ConcreteFunctor localMappings baseName params
        TypeParameter baseName paramName -> validateTypeParameterType @ConcreteFunctor localMappings baseName paramName
        _                                -> fatal $ ErrorNotAType _pathBase

    validateStructType baseName actualParams = do
      expectedParams <- retrieveStructParams baseName
      validateParamsCount baseName expectedParams actualParams
      pure $ StructType $ StructTypeInfo
        { _structBaseName   = baseName
        , _structTypeParams = actualParams
        }


validateNonEmptyPartialType
  :: Resolved.PathInfo
  -> Validate (TypeNode PartialFunctor)
validateNonEmptyPartialType path =
  validatePartialType path `onNothingM`
    fatal (ErrorPlaceholder "placeholder at root")

validatePartialType
  :: Resolved.PathInfo
  -> Validate PartialType
validatePartialType = go M.empty
  where
    go
      :: HashMap Identifier PartialType
      -> Resolved.PathInfo
      -> Validate PartialType
    go localMappings PathInfo {..} = do
      params <- traverse (go localMappings) _pathParams
      case _pathBase of
        BuiltinType name                 -> Just <$> validateBuiltinType name params
        Struct baseName                  -> validateStructType baseName params
        Enum baseName                    -> Just <$> validateEnumType baseName params
        TypeAlias baseName               -> validateTypeAliasType @PartialFunctor localMappings baseName params
        TypeParameter baseName paramName -> validateTypeParameterType @PartialFunctor localMappings baseName paramName
        Placeholder                      -> pure Nothing
        _                                -> fatal $ ErrorNotAType _pathBase

    validateStructType baseName actualParams = do
      expectedParams <- retrieveStructParams baseName
      validateParamsCountWith (<=) baseName expectedParams actualParams
      pure $ Just $ StructType $ StructTypeInfo
        { _structBaseName   = baseName
        , _structTypeParams = actualParams
        }

validateParameterizedType
  :: Resolved.PathInfo
  -> Validate ParameterizedType
validateParameterizedType = go M.empty
  where
    go
      :: HashMap Identifier ParameterizedType
      -> Resolved.PathInfo
      -> Validate ParameterizedType
    go localMappings PathInfo {..} = do
      params <- traverse (go localMappings) _pathParams
      case _pathBase of
        BuiltinType name ->
          Right <$> validateBuiltinType name params
        Struct baseName ->
          validateStructType baseName params
        Enum baseName ->
          Right <$> validateEnumType baseName params
        TypeAlias baseName ->
          validateTypeAliasType @ParameterizedFunctor localMappings baseName params
        TypeParameter _ paramName ->
          pure $ Left paramName
        Placeholder ->
          fatal $ ErrorPlaceholder unimplemented
        _ ->
          fatal $ ErrorNotAType _pathBase
    validateStructType baseName actualParams = do
      expectedParams <- retrieveStructParams baseName
      validateParamsCount baseName expectedParams actualParams
      pure $ Right $ StructType $ StructTypeInfo
        { _structBaseName   = baseName
        , _structTypeParams = actualParams
        }

validateBuiltinType
  :: HasCallStack
  => Name
  -> [TypeTree f]
  -> Validate (TypeNode f)
validateBuiltinType name params = case name of
  IntName  -> validateNoParams $> IntType
  CharName -> validateNoParams $> CharType
  BoolName -> validateNoParams $> BoolType
  UnitName -> validateNoParams $> UnitType
  VoidName -> pure VoidType
  _ ->
    reportICE
      "builtin type validation"
      "unknown builtin type"
      ["name: " ++ show name]
  where
    validateNoParams =
      validateParamsCount (_nameBase name) [] params

validateEnumType
  :: BaseName
  -> [TypeTree f]
  -> Validate (TypeNode f)
validateEnumType baseName params = do
  validateParamsCount baseName [] params
  values <- retrieveEnum baseName
  pure $ EnumType baseName values

validateTypeAliasType
  :: forall f
   . (Applicative f, Show (TypeTree f))
  => M.HashMap Identifier (TypeTree f)
  -> BaseName
  -> [TypeTree f]
  -> Validate (TypeTree f)
validateTypeAliasType localMappings baseName params = do
  Validated.TypeAliasInfo {..} <- retrieveTypeAlias baseName
  validateParamsCount baseName _aliasParams params
  let newMappings = M.fromList $ zip _aliasParams params
  pure $ reifyType @f (M.union newMappings localMappings) _aliasValue

validateTypeParameterType
  :: forall f
   . (Applicative f)
  => M.HashMap Identifier (TypeTree f)
  -> BaseName
  -> Identifier
  -> Validate (TypeTree f)
validateTypeParameterType localMappings baseName paramName =
  M.lookup paramName localMappings `onNothing`
    fmap adapt (retrieveTypeParameter baseName paramName)
  where
    adapt :: ConcreteType -> TypeTree f
    adapt = hpure @f . abstract @f pure

buildTypeParameterMap
  :: ParameterizedType
  -> ConcreteType
  -> Validate [(Identifier, ConcreteType)]
buildTypeParameterMap expected actual =
  go expected actual
  where
    errorMessage = ErrorIncompatibleType (parameterizedToPartial expected) actual
    go e a = case (e, a) of
      (Left identifier, _) ->
        pure [(identifier, a)]
      (Right (StructType structType), VoidType) -> do
        concat <$> traverse (uncurry go . (, VoidType)) (_structTypeParams structType)
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
        unless (name1 == name2) $
          fatal errorMessage
        pure []
      (Right (StructType structType1), StructType structType2) -> do
        unless (_structBaseName structType1 == _structBaseName structType2) $
          fatal errorMessage
        concat <$> zipWithM go (_structTypeParams structType1) (_structTypeParams structType2)
      _ ->
        fatal errorMessage

validateTypePattern
  :: PartialType
  -> ConcreteType
  -> Validate ()
validateTypePattern expected actual =
  go expected actual
  where
    errorMessage = ErrorIncompatibleType expected actual
    go e a = case (e, a) of
      (Nothing, _) -> pass
      (_, VoidType) -> pass
      (Just IntType, IntType) -> pass
      (Just BoolType, BoolType) -> pass
      (Just CharType, CharType) -> pass
      (Just UnitType, UnitType) -> pass
      (Just (EnumType name1 _), EnumType name2 _) -> do
        unless (name1 == name2) $
          fatal errorMessage
      (Just (StructType structType1), StructType structType2) -> do
        unless (_structBaseName structType1 == _structBaseName structType2) $
          fatal errorMessage
        zipWithM_ go (_structTypeParams structType1) (_structTypeParams structType2)
      _ ->
        fatal errorMessage

validateParamsCount
  :: BaseName
  -> [Identifier]
  -> [t]
  -> Validate ()
validateParamsCount =
  validateParamsCountWith (==)

validateParamsCountWith
  :: (Int -> Int -> Bool)
  -> BaseName
  -> [Identifier]
  -> [t]
  -> Validate ()
validateParamsCountWith cmp baseName expectedParams actualParams = do
  let actual   = length actualParams
      expected = length expectedParams
  unless (actual `cmp` expected) $
    fatal $ ErrorIncorrectTypeParameterCount baseName expected actual

concreteToPartial
  :: ConcreteType
  -> PartialType
concreteToPartial =
  ffrecur (Just . runIdentity)

parameterizedToPartial
  :: ParameterizedType
  -> PartialType
parameterizedToPartial =
  ffrecur eitherToMaybe

concretizeType
  :: PartialType
  -> Maybe ConcreteType
concretizeType = go
  where
    go = \case
      Nothing ->
        Nothing
      Just IntType ->
        Just IntType
      Just BoolType ->
        Just BoolType
      Just CharType ->
        Just CharType
      Just UnitType ->
        Just UnitType
      Just VoidType ->
        Just VoidType
      Just (EnumType name values) ->
        Just $ EnumType name values
      Just (StructType structInfo) ->
        StructType <$> concretizeStructType structInfo
      Just (FunctionType functionInfo) ->
        FunctionType <$> concretizeFunctionType functionInfo

    concretizeFunctionType FunctionTypeInfo {..} = do
      concreteArgs   <- traverse3 go _funArgs
      concreteReturn <- go _funReturn
      pure $ FunctionTypeInfo _funParams concreteArgs concreteReturn

    concretizeStructType StructTypeInfo {..} = do
      concreteTypeParams <- traverse go _structTypeParams
      pure $ StructTypeInfo _structBaseName concreteTypeParams

reifyType
  :: forall f
   . (HasCallStack, Applicative f, Show (TypeTree f))
  => HashMap Identifier (TypeTree f)
  -> ParameterizedType
  -> TypeTree f
reifyType mappings = \case
  -- TODO: rewrite as ffrecur
  Left typeParameter ->
    lookupParameter typeParameter
  Right IntType ->
    raise IntType
  Right BoolType ->
    raise BoolType
  Right CharType ->
    raise CharType
  Right UnitType ->
    raise UnitType
  Right VoidType ->
    raise VoidType
  Right (EnumType name values) ->
    raise $ EnumType name values
  Right (StructType structInfo) ->
    raise $ StructType $ reifyStructType structInfo
  Right (FunctionType functionInfo) ->
    raise $ FunctionType $ reifyFunctionType functionInfo
  where
    raise :: TypeNode f -> TypeTree f
    raise = hpure @f

    reifyFunctionType FunctionTypeInfo {..} = FunctionTypeInfo
      { _funParams = []
      , _funArgs   = fmap3 (reifyType @f mappings) _funArgs
      , _funReturn = reifyType @f mappings _funReturn
      }

    reifyStructType =
      structTypeParams %~ fmap (reifyType @f mappings)

    lookupParameter :: Identifier -> TypeTree f
    lookupParameter identifier =
      flip fromMaybe (M.lookup identifier mappings) $
        reportICE
          "type reification"
          "no information for type parameter"
          [ "parameter name:   " ++ show identifier
          , "known parameters: " ++ show mappings
          ]
