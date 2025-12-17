{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lang.Pietre.Representations.AST.Validated
  ( module Lang.Pietre.Representations.AST.Validated
  , module Common
  ) where

import "this" Prelude

import Control.Lens
import Data.Kind

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.HKT
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Name

import Lang.Pietre.Representations.AST.Common as Common (ASTPhase (..),
                                                         ASTRepresentation (..),
                                                         CommonBlock,
                                                         CommonElseInfo (..),
                                                         CommonIfInfo (..),
                                                         CommonStatement (..),
                                                         CommonWhileInfo (..),
                                                         EnumInfo (..))


--------------------------------------------------------------------------------
-- AST Representation

instance ASTRepresentation Validated where
  type PathBodyType   Validated = Void
  type ExpressionType Validated = Typed Expression
  type ForInfoType    Validated = ForInfo
  type LetInfoType    Validated = LetInfo


--------------------------------------------------------------------------------
-- Types

type ConcreteFunctor      = Identity
type PartialFunctor       = Maybe
type ParameterizedFunctor = Either (BaseName, Identifier)
type ConcreteType         = TypeTree ConcreteFunctor
type PartialType          = TypeTree PartialFunctor
type ParameterizedType    = TypeTree ParameterizedFunctor

type TypeTree (f :: Type -> Type) = HKT f (TypeNode f)

data TypeNode f
  = IntType
  | BoolType
  | CharType
  | UnitType
  | VoidType
  | EnumType BaseName [Identifier]
  | StructType (StructTypeInfo f)
  | FunctionType (FunctionTypeInfo f)

deriving instance (Show (HKT f (TypeNode f))) => Show (TypeNode f)

instance FFunctor TypeNode where
  ffmap f = \case
    IntType -> IntType
    BoolType -> BoolType
    CharType -> CharType
    UnitType -> UnitType
    VoidType -> VoidType
    EnumType n cs -> EnumType n cs
    StructType StructTypeInfo {..} ->
      StructType $ StructTypeInfo
        _structBaseName
        (ffrecur @TypeNode f <$> _structTypeParams)
    FunctionType FunctionTypeInfo {..} ->
      FunctionType $ FunctionTypeInfo
        _funParams
        (fmap2 ffmapFunArgs _funArgs)
        (ffrecur @TypeNode f _funReturn)
    where
      ffmapFunArgs = \case
        ByReference t -> ByReference $ ffrecur @TypeNode f t
        ByValue     t -> ByValue     $ ffrecur @TypeNode f t

data StructTypeInfo f = StructTypeInfo
  { _structBaseName   :: BaseName
  , _structTypeParams :: [TypeTree f]
  }

deriving instance Show (HKT f (TypeNode f)) => Show (StructTypeInfo f)

typeName :: ConcreteType -> Maybe Name
typeName = \case
  IntType  ->
    Just IntName
  BoolType ->
    Just BoolName
  CharType ->
    Just CharName
  UnitType ->
    Just UnitName
  VoidType ->
    Just UnitName
  EnumType baseName _ ->
    Just $ Name baseName []
  StructType StructTypeInfo {..} ->
    Name _structBaseName <$> traverse typeName _structTypeParams
  FunctionType _ ->
    Nothing

assertName :: HasCallStack => ConcreteType -> Name
assertName t = fromMaybe raiseError $ typeName t
  where
    raiseError =
      reportICE
        "name assertion"
        "name not found for given type"
        ["type: " ++ show t]


--------------------------------------------------------------------------------
-- Definitions

data Definition
  = TypeAliasDef TypeAliasInfo
  | EnumDef      EnumInfo
  | StructDef    (StructInfo ParameterizedFunctor)
  | ConstDef     (Typed ConstExpression)
  | FunctionDef  (FunctionTypeInfo ParameterizedFunctor)
  deriving Show

data TypeAliasInfo = TypeAliasInfo
  { _aliasParams :: [Identifier]
  , _aliasValue  :: ParameterizedType
  }
  deriving Show

data StructInfo f = StructInfo
  { _structParams :: [Identifier]
  , _structValues :: NonEmpty (Identifier, TypeTree f)
  }

deriving instance Show (HKT f (TypeNode f)) => Show (StructInfo f)


data FunctionInfo = FunctionInfo
  { _funType :: FunctionTypeInfo ConcreteFunctor
  , _funBody :: Block
  }
  deriving Show

data FunctionTypeInfo f = FunctionTypeInfo
  { _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, FunctionArgType f)]
  , _funReturn :: TypeTree f
  }

deriving instance Show (HKT f (TypeNode f)) => Show (FunctionTypeInfo f)

data FunctionArgType f
  = ByValue     (TypeTree f)
  | ByReference (TypeTree f)

deriving instance Show (HKT f (TypeNode f)) => Show (FunctionArgType f)

functionArgInnerType :: FunctionArgType f -> TypeTree f
functionArgInnerType = \case
  ByValue     t -> t
  ByReference t -> t

data ForInfo = ForInfo
  { _forVariableName :: Identifier
  , _forVariableType :: ConcreteType
  , _forRangeExpr    :: RangeExpression
  , _forBody         :: Block
  }
  deriving Show

data LetInfo = LetInfo
  { _letName  :: Identifier
  , _letValue :: Typed Expression
  }
  deriving Show

data Typed a = Typed
  { _typeInfo   :: ConcreteType
  , _typedValue :: a
  }
  deriving (Show, Functor, Foldable, Traversable)

data ConstExpression
  = ArrayConstExpr         [Typed ConstExpression]
  | StructConstExpr        (StructInfo ConcreteFunctor) (NonEmpty (Identifier, Typed ConstExpression))
  | BoolLiteralConstExpr   Bool
  | IntLiteralConstExpr    Int
  | CharLiteralConstExpr   Char
  | StringLiteralConstExpr Text
  deriving Show

data Expression
  = LocalVariableExpr            Identifier
  | ReferenceArgumentExpr        Identifier
  | IndexExpr                    (Typed Expression) (Typed Expression)
  | FunctionNameExpr             Name (FunctionTypeInfo ConcreteFunctor)
  | FunctionCallExpr             Name (FunctionTypeInfo ConcreteFunctor) [Typed Expression]
  | VariableCallExpr             Identifier (FunctionTypeInfo ConcreteFunctor) [Typed Expression]
  | ArrayExpr                    [Typed Expression]
  | StructExpr                   (StructInfo ConcreteFunctor) (NonEmpty (Identifier, Typed Expression))
  | FieldAccessExpr              (StructInfo ConcreteFunctor) (Typed Expression) Identifier
  | BoolLiteralExpr              Bool
  | IntLiteralExpr               Int
  | CharLiteralExpr              Char
  | StringLiteralExpr            Text
  | IntNegationExpr              (Typed Expression)
  | BoolNegationExpr             (Typed Expression)
  | AdditionExpr                 (Typed Expression) (Typed Expression)
  | SubtractionExpr              (Typed Expression) (Typed Expression)
  | MultiplicationExpr           (Typed Expression) (Typed Expression)
  | DivisionExpr                 (Typed Expression) (Typed Expression)
  | ModuloExpr                   (Typed Expression) (Typed Expression)
  | ExponentiationExpr           (Typed Expression) (Typed Expression)
  | EqualityExpr                 (Typed Expression) (Typed Expression)
  | DifferenceExpr               (Typed Expression) (Typed Expression)
  | GreaterExpr                  (Typed Expression) (Typed Expression)
  | LesserExpr                   (Typed Expression) (Typed Expression)
  | GreaterEqExpr                (Typed Expression) (Typed Expression)
  | LesserEqExpr                 (Typed Expression) (Typed Expression)
  | BoolAndExpr                  (Typed Expression) (Typed Expression)
  | BoolOrExpr                   (Typed Expression) (Typed Expression)
  | CastExpr                     (Typed Expression) ConcreteType
  | RangeExpr                    (Typed RangeExpression)
  | AssignmentExpr               (Typed LValueExpression) (Typed Expression)
  | AdditionAssignmentExpr       (Typed LValueExpression) (Typed Expression)
  | SubtractionAssignmentExpr    (Typed LValueExpression) (Typed Expression)
  | MultiplicationAssignmentExpr (Typed LValueExpression) (Typed Expression)
  | DivisionAssignmentExpr       (Typed LValueExpression) (Typed Expression)
  | ModuloAssignmentExpr         (Typed LValueExpression) (Typed Expression)
  | ExponentiationAssignmentExpr (Typed LValueExpression) (Typed Expression)
  deriving Show

data RangeExpression
  = RangeInclusiveExpr (Typed Expression) (Typed Expression)
  | RangeExclusiveExpr (Typed Expression) (Typed Expression)
  deriving Show

data LValueExpression
  = LocalVariableLExpr     Identifier
  | ReferenceArgumentLExpr Identifier
  | FieldAccessLExpr       (StructInfo ConcreteFunctor) (Typed LValueExpression) Identifier
  | IndexLExpr             (Typed LValueExpression) (Typed LValueExpression)
  deriving Show


--------------------------------------------------------------------------------
-- Re-exports

type IfInfo    = CommonIfInfo    Validated
type ElseInfo  = CommonElseInfo  Validated
type Statement = CommonStatement Validated
type WhileInfo = CommonWhileInfo Validated
type Block     = CommonBlock     Validated


--------------------------------------------------------------------------------
-- Lenses

makeLenses ''StructTypeInfo
makeLenses ''TypeAliasInfo
makeLenses ''StructInfo
makeLenses ''FunctionInfo
makeLenses ''ForInfo
makeLenses ''LetInfo

makePrisms ''Definition
makePrisms ''FunctionArgType
makePrisms ''Expression
makePrisms ''ConstExpression
makePrisms ''LValueExpression

instance Plated ConstExpression where
  plate f = \case
    ArrayConstExpr  xs    -> ArrayConstExpr     <$> traverse2 f xs
    StructConstExpr si fs -> StructConstExpr si <$> traverse3 f fs
    leaf                  -> pure leaf

instance Plated Expression where
  plate f = \case
    FunctionCallExpr   n t xs     -> FunctionCallExpr n t <$> traverse2 f xs
    VariableCallExpr   n t xs     -> VariableCallExpr n t <$> traverse2 f xs
    ArrayExpr          xs         -> ArrayExpr <$> traverse2 f xs
    StructExpr         si fields  -> StructExpr si <$> traverse3 f fields
    FieldAccessExpr    si expr fn -> liftA2 (FieldAccessExpr si) (traverse f expr) (pure fn)
    IntNegationExpr    expr       -> IntNegationExpr  <$> traverse f expr
    BoolNegationExpr   expr       -> BoolNegationExpr <$> traverse f expr
    CastExpr           lhs t      -> liftA2 CastExpr           (traverse f lhs) (pure t)
    IndexExpr          lhs rhs    -> liftA2 IndexExpr          (traverse f lhs) (traverse f rhs)
    AdditionExpr       lhs rhs    -> liftA2 AdditionExpr       (traverse f lhs) (traverse f rhs)
    SubtractionExpr    lhs rhs    -> liftA2 SubtractionExpr    (traverse f lhs) (traverse f rhs)
    MultiplicationExpr lhs rhs    -> liftA2 MultiplicationExpr (traverse f lhs) (traverse f rhs)
    DivisionExpr       lhs rhs    -> liftA2 DivisionExpr       (traverse f lhs) (traverse f rhs)
    ModuloExpr         lhs rhs    -> liftA2 ModuloExpr         (traverse f lhs) (traverse f rhs)
    ExponentiationExpr lhs rhs    -> liftA2 ExponentiationExpr (traverse f lhs) (traverse f rhs)
    EqualityExpr       lhs rhs    -> liftA2 EqualityExpr       (traverse f lhs) (traverse f rhs)
    DifferenceExpr     lhs rhs    -> liftA2 DifferenceExpr     (traverse f lhs) (traverse f rhs)
    GreaterExpr        lhs rhs    -> liftA2 GreaterExpr        (traverse f lhs) (traverse f rhs)
    LesserExpr         lhs rhs    -> liftA2 LesserExpr         (traverse f lhs) (traverse f rhs)
    GreaterEqExpr      lhs rhs    -> liftA2 GreaterEqExpr      (traverse f lhs) (traverse f rhs)
    LesserEqExpr       lhs rhs    -> liftA2 LesserEqExpr       (traverse f lhs) (traverse f rhs)
    BoolAndExpr        lhs rhs    -> liftA2 BoolAndExpr        (traverse f lhs) (traverse f rhs)
    BoolOrExpr         lhs rhs    -> liftA2 BoolOrExpr         (traverse f lhs) (traverse f rhs)
    leaf                          -> pure leaf

instance Plated LValueExpression where
  plate f = \case
    FieldAccessLExpr si expr fn -> liftA2 (FieldAccessLExpr si) (traverse f expr) (pure fn)
    IndexLExpr       lhs rhs    -> liftA2 IndexLExpr (traverse f lhs) (traverse f rhs)
    leaf                        -> pure leaf
