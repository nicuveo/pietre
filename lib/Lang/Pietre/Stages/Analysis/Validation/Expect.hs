module Lang.Pietre.Stages.Analysis.Validation.Expect where

import "this" Prelude

import Data.List                                    qualified as L

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved     qualified as Resolved
import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Monad


assertConstant
  :: HasCallStack
  => BaseName
  -> Definition
  -> Validate (Typed ConstExpression)
assertConstant baseName = \case
  ConstDef constInfo -> pure constInfo
  -- TODO: explain why this is needed
  EnumDef enumInfo ->
    case L.elemIndex (_nameIdent baseName) (_enumValues enumInfo) of
      Just i ->
        pure $ Typed IntType $ IntLiteralConstExpr i
      Nothing ->
        reportICE
          "enum value resolution"
          "unknown enum constructor"
          [ "name: " ++ show baseName
          , "enum: " ++ show enumInfo
          ]
  definition -> reportICE
    "const definition retrieval"
    "definition not a const"
    ["definition: " ++ show definition]

assertTypeAlias
  :: HasCallStack
  => Definition
  -> Validate TypeAliasInfo
assertTypeAlias = \case
  TypeAliasDef typeAliasInfo -> pure typeAliasInfo
  definition -> reportICE
    "type alias definition retrieval"
    "definition not a type alias"
    ["definition: " ++ show definition]

assertEnum
  :: HasCallStack
  => Definition
  -> Validate [Identifier]
assertEnum = \case
  EnumDef enumInfo -> pure $ _enumValues enumInfo
  definition -> reportICE
    "enum definition retrieval"
    "definition not an enum"
    ["definition: " ++ show definition]

assertStruct
  :: HasCallStack
  => Definition
  -> Validate (StructInfo ParameterizedFunctor)
assertStruct = \case
  StructDef structInfo -> pure structInfo
  definition -> reportICE
    "struct definition retrieval"
    "definition not a struct"
    ["definition: " ++ show definition]

assertStructDefinition
  :: HasCallStack
  => Resolved.Definition
  -> Validate Resolved.StructInfo
assertStructDefinition = \case
  Resolved.StructDef structInfo -> pure structInfo
  definition -> reportICE
    "struct definition retrieval"
    "definition not a struct"
    ["definition: " ++ show definition]

assertFunctionType
  :: HasCallStack
  => Definition
  -> Validate (FunctionTypeInfo ParameterizedFunctor)
assertFunctionType = \case
  FunctionDef functionType -> pure functionType
  definition -> reportICE
    "function type retrieval"
    "definition not a function"
    ["definition: " ++ show definition]

expectType
  :: ConcreteType
  -> ConcreteType
  -> Validate ()
expectType expected actual =
  unless (expected `typeMatches` actual) $
    fatal $ ErrorWrongType [expected] actual

expectTypeOneOf
  :: [ConcreteType]
  -> ConcreteType
  -> Validate ()
expectTypeOneOf expected actual =
  unless (any (`typeMatches` actual) expected) $
    fatal $ ErrorWrongType expected actual

expectConstInt
  :: HasCallStack
  => Typed ConstExpression
  -> Validate Int
expectConstInt Typed {..} = do
  expectType IntType _typeInfo
  case _typedValue of
    IntLiteralConstExpr i -> pure i
    _ -> reportICE "const expr validation" "not a literal int value" ["value: " ++ show _typedValue]

expectConstBool
  :: HasCallStack
  => Typed ConstExpression
  -> Validate Bool
expectConstBool Typed {..} = do
  expectType BoolType _typeInfo
  case _typedValue of
    BoolLiteralConstExpr i -> pure i
    _ -> reportICE "const expr validation" "not a literal bool value" ["value: " ++ show _typedValue]

typeMatches
  :: ConcreteType
  -> ConcreteType
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
    (FunctionType f1, FunctionType f2) -> functionsMatch f1 f2
    _                                  -> False
  where
    structsMatch s1 s2 =
      (_structBaseName s1 == _structBaseName s2) &&
      and (zipWith typeMatches (_structTypeParams s1) (_structTypeParams s2))

    functionsMatch f1 f2 =
      (_funReturn f1 `typeMatches` _funReturn f2) &&
      and (zipWith (functionArgsMatch `on` snd) (_funArgs f1) (_funArgs f2))

    functionArgsMatch = curry \case
      (ByReference t1, ByReference t2) -> t1 `typeMatches` t2
      (ByValue     t1, ByValue     t2) -> t1 `typeMatches` t2
      _ -> False

typesAllMatch
  :: [ConcreteType]
  -> Bool
typesAllMatch types = and do
  (headType : remainingTypes) <- L.tails types
  otherType <- remainingTypes
  pure $ headType `typeMatches` otherType
