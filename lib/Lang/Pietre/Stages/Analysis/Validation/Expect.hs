module Lang.Pietre.Stages.Analysis.Validation.Expect where

import "this" Prelude

import Control.Lens                                 hiding (mapping, op)
import Control.Monad.Loops                          (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                    (hoistMaybe)
import Data.HashMap.Strict.Extra                    qualified as M
import Data.HashSet                                 qualified as S
import Data.List                                    qualified as L
import Data.Ordered.Set                             qualified as OSet
import Data.Set                                     qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Resolved
import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Monad


assertConst
  :: Monad m
  => BaseName
  -> Definition Validated
  -> ValidateT m (Typed ConstExpr)
assertConst baseName = \case
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
          [ "name: " ++ show name
          , "enum: " ++ show enumIfo
          ]
  definition -> reportICE
    "const definition retrieval"
    "definition not a const"
    ["definition: " ++ definition]

assertTypeAlias
  :: Monad m
  => Definition Validated
  -> ValidateT m TypeAliasInfo
assertTypeAlias = \case
  TypeAliasDef typeAliasInfo -> pure typeAliasInfo
  definition -> reportICE
    "type alias definition retrieval"
    "definition not a type alias"
    ["definition: " ++ definition]

assertEnum
  :: Monad m
  => Definition Validated
  -> ValidateT m [Identifier]
assertEnum = \case
  EnumDef enumInfo -> pure $ _enumValues enumInfo
  definition -> reportICE
    "enum definition retrieval"
    "definition not an enum"
    ["definition: " ++ definition]

assertStruct
  :: Monad m
  => Definition Validated
  -> ValidateT m (StructInfo Validated)
assertStruct = \case
  StructDef constInfo -> pure constInfo
  definition -> reportICE
    "struct definition retrieval"
    "definition not a struct"
    ["definition: " ++ definition]

assertFunctionType
  :: Monad m
  => Definition Validated
  -> ValidateT m (FunctionTypeInfo ParameterizedFunctor)
assertFunctionType = \case
  FunctionDef functionType -> pure functionType
  definition -> reportICE
    "function type retrieval"
    "definition not a function"
    ["definition: " ++ definition]

assertFunctionDefinition
  :: Monad m
  => Definition Validated
  -> ValidateT m Resolved.FunctionInfo
assertFunctionDefinition = \case
  FunctionDef functionInfo -> pure functionInfo
  definition -> reportICE
    "function definition retrieval"
    "definition not a function"
    ["definition: " ++ definition]

expectType
  :: Monad m
  => Type
  -> Type
  -> ValidateT m ()
expectType expected actual =
  unless (expected `typeMatches` actual) $
    report $ ErrorWrongType [expected] actual

expectTypeOneOf
  :: Monad m
  => [Type]
  -> Type
  -> ValidateT m ()
expectTypeOneOf expected actual =
  unless (any (`typeMatches` actual) expected) $
    report $ ErrorWrongType expected actual

expectConstInt
  :: Monad m
  => Typed ConstExpression
  -> ValidateT m Int
expectConstInt Typed {..} = do
  expectType IntType _typeInfo
  case _typedValue of
    IntLiteralConstExpr i -> pure i
    _ -> reportICE "const expr validation" "not a literal int value" ["value: " ++ show _typedValue]

expectConstBool
  :: Monad m
  => Typed ConstExpression
  -> ValidateT m Bool
expectConstBool Typed {..} = do
  expectType BoolType _typeInfo
  case _typedValue of
    BoolLiteralConstExpr i -> pure i
    _ -> reportICE "const expr validation" "not a literal bool value" ["value: " ++ show _typedValue]
