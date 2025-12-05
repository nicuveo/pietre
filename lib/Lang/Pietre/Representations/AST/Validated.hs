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
                                                         EnumInfo (..),
                                                         FunctionArgType (..))


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
        (fmap3 (ffrecur @TypeNode f) _funArgs)
        (ffrecur @TypeNode f _funReturn)

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
  , _funArgs   :: [(Identifier, FunctionArgType (TypeTree f))]
  , _funReturn :: TypeTree f
  }

deriving instance Show (HKT f (TypeNode f)) => Show (FunctionTypeInfo f)

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

pattern IntConstExpr :: Int -> Typed ConstExpression
pattern IntConstExpr i = Typed IntType (IntLiteralConstExpr i)

pattern CharConstExpr :: Char -> Typed ConstExpression
pattern CharConstExpr c = Typed CharType (CharLiteralConstExpr c)

pattern BoolConstExpr :: Bool -> Typed ConstExpression
pattern BoolConstExpr b = Typed BoolType (BoolLiteralConstExpr b)


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

pattern IntExpr :: Int -> Typed Expression
pattern IntExpr i = Typed IntType (IntLiteralExpr i)

pattern CharExpr :: Char -> Typed Expression
pattern CharExpr c = Typed CharType (CharLiteralExpr c)

pattern BoolExpr :: Bool -> Typed Expression
pattern BoolExpr b = Typed BoolType (BoolLiteralExpr b)


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
makeLenses ''Typed

makePrisms ''Definition
makePrisms ''Expression
makePrisms ''ConstExpression
makePrisms ''LValueExpression

instance Plated (Typed ConstExpression) where
  plate f Typed {..} = Typed _typeInfo <$> case _typedValue of
    ArrayConstExpr  xs    -> ArrayConstExpr     <$> traverse f xs
    StructConstExpr si fs -> StructConstExpr si <$> traverse2 f fs
    leaf                  -> pure leaf

instance Plated (Typed Expression) where
  plate f Typed {..} = Typed _typeInfo <$> case _typedValue of
    FunctionCallExpr   n t xs     -> FunctionCallExpr n t <$> traverse f xs
    VariableCallExpr   n t xs     -> VariableCallExpr n t <$> traverse f xs
    ArrayExpr          xs         -> ArrayExpr <$> traverse f xs
    StructExpr         si fields  -> StructExpr si <$> traverse2 f fields
    FieldAccessExpr    si expr fn -> liftA2 (FieldAccessExpr si) (f expr) (pure fn)
    IntNegationExpr    expr       -> IntNegationExpr  <$> f expr
    BoolNegationExpr   expr       -> BoolNegationExpr <$> f expr
    CastExpr           lhs t      -> liftA2 CastExpr           (f lhs) (pure t)
    IndexExpr          lhs rhs    -> liftA2 IndexExpr          (f lhs) (f rhs)
    AdditionExpr       lhs rhs    -> liftA2 AdditionExpr       (f lhs) (f rhs)
    SubtractionExpr    lhs rhs    -> liftA2 SubtractionExpr    (f lhs) (f rhs)
    MultiplicationExpr lhs rhs    -> liftA2 MultiplicationExpr (f lhs) (f rhs)
    DivisionExpr       lhs rhs    -> liftA2 DivisionExpr       (f lhs) (f rhs)
    ModuloExpr         lhs rhs    -> liftA2 ModuloExpr         (f lhs) (f rhs)
    ExponentiationExpr lhs rhs    -> liftA2 ExponentiationExpr (f lhs) (f rhs)
    EqualityExpr       lhs rhs    -> liftA2 EqualityExpr       (f lhs) (f rhs)
    DifferenceExpr     lhs rhs    -> liftA2 DifferenceExpr     (f lhs) (f rhs)
    GreaterExpr        lhs rhs    -> liftA2 GreaterExpr        (f lhs) (f rhs)
    LesserExpr         lhs rhs    -> liftA2 LesserExpr         (f lhs) (f rhs)
    GreaterEqExpr      lhs rhs    -> liftA2 GreaterEqExpr      (f lhs) (f rhs)
    LesserEqExpr       lhs rhs    -> liftA2 LesserEqExpr       (f lhs) (f rhs)
    BoolAndExpr        lhs rhs    -> liftA2 BoolAndExpr        (f lhs) (f rhs)
    BoolOrExpr         lhs rhs    -> liftA2 BoolOrExpr         (f lhs) (f rhs)
    leaf                          -> pure leaf

instance Plated (Typed LValueExpression) where
  plate f Typed {..} = Typed _typeInfo <$> case _typedValue of
    FieldAccessLExpr si expr fn -> liftA2 (FieldAccessLExpr si) (f expr) (pure fn)
    IndexLExpr       lhs rhs    -> liftA2 IndexLExpr (f lhs) (f rhs)
    leaf                        -> pure leaf
