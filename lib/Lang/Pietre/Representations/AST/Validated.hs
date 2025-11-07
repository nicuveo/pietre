{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Representations.AST where

import                "this" Prelude

import                Control.Lens
import                Data.Kind
import                Data.List.NonEmpty                     qualified as NE
import                Prettyprinter
import                Prettyprinter.Render.Text

import                Lang.Pietre.Representations.Identifier
import                Lang.Pietre.Representations.Location
import {-# SOURCE #-} Lang.Pietre.Representations.Name


data Type
  = IntType
  | BoolType
  | CharType
  | UnitType
  | VoidType
  | EnumType Name [Identifier]
  | StructType Name (NonEmpty (Identifier, Name))
  | FunctionType FunctionType
  deriving Show

data StructInfo = StructInfo
  { _structParams :: [Identifier]
  , _structFields :: _
  }

data FunctionType = FunctionInfo
  { _funArgs   :: [FunctionArgType]
  , _funReturn :: Maybe Type
  }

data FunctionArgType
  = ByValue     Name
  | ByReference Name
  deriving (Show, Generic)

data FunctionInfo = FunctionInfo
  { _funType :: FunctionType
  , _funBody :: Block
  }

data Statement
  = IfStmt         IfInfo
  | ForStmt        ForInfo
  | WhileStmt      WhileInfo
  | LetStmt        LetInfo
  | ReturnStmt     Maybe TypedExpression
  | ContinueStmt
  | BreakStmt
  | ExpressionStmt TypedExpression
  deriving Show

type Block = [WithLocation Statement]

data IfInfo = IfInfo
  { _ifExpr :: Expression
  , _ifBody :: Block
  , _ifElse :: Maybe ElseInfo
  }
  deriving Show

data ElseInfo
  = ElseIf    IfInfo
  | ElseBlock Block
  deriving Show

data ForInfo = ForInfo
  { _forVariableName :: Identifier
  , _forVariableType :: Type
  , _forRangeExpr    :: RangeExpression
  , _forBody         :: Block
  }
  deriving Show

data WhileInfo = WhileInfo
  { _whileExpr :: Expression
  , _whileBody :: Block
  }
  deriving Show

data LetInfo = LetInfo
  { _letName  :: Identifier
  , _letType  :: Type
  , _letValue :: Expression
  }
  deriving Show

data Typed a = Typed
  { _typeInfo   :: Type
  , _typedValue :: a
  }
  deriving (Show, Functor, Applicative, Monad)

data ConstExpression
  = ArrayConstExpr         [Typed ConstExpression]
  | StructConstExpr        Name StructInfo (NonEmpty (Identifier, Typed ConstExpression))
  | BoolLiteralConstExpr   Bool
  | IntLiteralConstExpr    Int
  | CharLiteralConstExpr   Char
  | StringLiteralConstExpr Text
  deriving Show

data Expression
  = LocalVariableExpr            Identifier
  | ReferenceArgumentExpr        Identifier
  | IndexExpr                    (Typed Expression) (Typed Expression)
  | FunctionNameExpr             Name FunctionType
  | CallExpr                     Name FunctionType [Typed Expression]
  | ArrayExpr                    [Typed Expression]
  | StructExpr                   Name StructInfo (NonEmpty (Identifier, Typed Expression))
  | FieldAccessExpr              Name StructInfo (Typed Expression) Identifier
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
  | CastExpr                     (Typed Expression) Type
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
  | FieldAccessLExpr       Name StructInfo (Typed LValueExpression) Identifier
  | IndexLExpr             (Typed LValueExpression) (Typed LValueExpression)
  deriving Show


--------------------------------------------------------------------------------
-- Lenses

makeLenses ''Module
makeLenses ''Import
makeLenses ''PathInfo
makeLenses ''TypeAliasInfo
makeLenses ''EnumInfo
makeLenses ''StructInfo
makeLenses ''ConstInfo
makeLenses ''FunctionInfo
makeLenses ''IfInfo
makeLenses ''ForInfo
makeLenses ''WhileInfo
makeLenses ''LetInfo
makeLenses ''TypedExpression

makePrisms ''ImportType
makePrisms ''Definition
makePrisms ''FunctionArgType
makePrisms ''Statement
makePrisms ''ElseInfo
makePrisms ''Expression

instance Annotation Expression p => Plated (Expression p) where
  plate f = \case
    FieldAccessExpr              e i   -> liftA2 FieldAccessExpr              (within f e) (pure i)
    CallExpr                     p es  -> liftA2 CallExpr                     (pure p) (traverse (within f) es)
    ArrayExpr                    es    -> fmap   ArrayExpr                    (traverse (within f) es)
    IndexExpr                    e1 e2 -> liftA2 IndexExpr                    (within f e1) (within f e2)
    StructExpr                   p fs  -> liftA2 StructExpr                   (pure p) (traverse (traverse (within f)) fs)
    IntNegationExpr              e     -> fmap   IntNegationExpr              (within f e)
    BoolNegationExpr             e     -> fmap   BoolNegationExpr             (within f e)
    CastExpr                     e t   -> liftA2 CastExpr                     (within f e) (pure t)
    AdditionExpr                 e1 e2 -> liftA2 AdditionExpr                 (within f e1) (within f e2)
    SubtractionExpr              e1 e2 -> liftA2 SubtractionExpr              (within f e1) (within f e2)
    MultiplicationExpr           e1 e2 -> liftA2 MultiplicationExpr           (within f e1) (within f e2)
    DivisionExpr                 e1 e2 -> liftA2 DivisionExpr                 (within f e1) (within f e2)
    ModuloExpr                   e1 e2 -> liftA2 ModuloExpr                   (within f e1) (within f e2)
    ExponentiationExpr           e1 e2 -> liftA2 ExponentiationExpr           (within f e1) (within f e2)
    EqualityExpr                 e1 e2 -> liftA2 EqualityExpr                 (within f e1) (within f e2)
    DifferenceExpr               e1 e2 -> liftA2 DifferenceExpr               (within f e1) (within f e2)
    GreaterExpr                  e1 e2 -> liftA2 GreaterExpr                  (within f e1) (within f e2)
    LesserExpr                   e1 e2 -> liftA2 LesserExpr                   (within f e1) (within f e2)
    GreaterEqExpr                e1 e2 -> liftA2 GreaterEqExpr                (within f e1) (within f e2)
    LesserEqExpr                 e1 e2 -> liftA2 LesserEqExpr                 (within f e1) (within f e2)
    BoolAndExpr                  e1 e2 -> liftA2 BoolAndExpr                  (within f e1) (within f e2)
    BoolOrExpr                   e1 e2 -> liftA2 BoolOrExpr                   (within f e1) (within f e2)
    RangeInclusiveExpr           e1 e2 -> liftA2 RangeInclusiveExpr           (within f e1) (within f e2)
    RangeExclusiveExpr           e1 e2 -> liftA2 RangeExclusiveExpr           (within f e1) (within f e2)
    AssignmentExpr               e1 e2 -> liftA2 AssignmentExpr               (within f e1) (within f e2)
    AdditionAssignmentExpr       e1 e2 -> liftA2 AdditionAssignmentExpr       (within f e1) (within f e2)
    SubtractionAssignmentExpr    e1 e2 -> liftA2 SubtractionAssignmentExpr    (within f e1) (within f e2)
    MultiplicationAssignmentExpr e1 e2 -> liftA2 MultiplicationAssignmentExpr (within f e1) (within f e2)
    DivisionAssignmentExpr       e1 e2 -> liftA2 DivisionAssignmentExpr       (within f e1) (within f e2)
    ModuloAssignmentExpr         e1 e2 -> liftA2 ModuloAssignmentExpr         (within f e1) (within f e2)
    ExponentiationAssignmentExpr e1 e2 -> liftA2 ExponentiationAssignmentExpr (within f e1) (within f e2)
    e                                  -> pure e

instance Plated TypedExpression where
  plate f TypedExpression {..} = TypedExpression _exprIsLValue _exprPurity _exprType <$> case _exprValue of
    FieldAccessExpr              e i   -> liftA2 FieldAccessExpr              (f e) (pure i)
    CallExpr                     p es  -> liftA2 CallExpr                     (pure p) (traverse f es)
    ArrayExpr                    es    -> fmap   ArrayExpr                    (traverse f es)
    IndexExpr                    e1 e2 -> liftA2 IndexExpr                    (f e1) (f e2)
    StructExpr                   p fs  -> liftA2 StructExpr                   (pure p) (traverse (traverse f) fs)
    IntNegationExpr              e     -> fmap   IntNegationExpr              (f e)
    BoolNegationExpr             e     -> fmap   BoolNegationExpr             (f e)
    CastExpr                     e t   -> liftA2 CastExpr                     (f e) (pure t)
    AdditionExpr                 e1 e2 -> liftA2 AdditionExpr                 (f e1) (f e2)
    SubtractionExpr              e1 e2 -> liftA2 SubtractionExpr              (f e1) (f e2)
    MultiplicationExpr           e1 e2 -> liftA2 MultiplicationExpr           (f e1) (f e2)
    DivisionExpr                 e1 e2 -> liftA2 DivisionExpr                 (f e1) (f e2)
    ModuloExpr                   e1 e2 -> liftA2 ModuloExpr                   (f e1) (f e2)
    ExponentiationExpr           e1 e2 -> liftA2 ExponentiationExpr           (f e1) (f e2)
    EqualityExpr                 e1 e2 -> liftA2 EqualityExpr                 (f e1) (f e2)
    DifferenceExpr               e1 e2 -> liftA2 DifferenceExpr               (f e1) (f e2)
    GreaterExpr                  e1 e2 -> liftA2 GreaterExpr                  (f e1) (f e2)
    LesserExpr                   e1 e2 -> liftA2 LesserExpr                   (f e1) (f e2)
    GreaterEqExpr                e1 e2 -> liftA2 GreaterEqExpr                (f e1) (f e2)
    LesserEqExpr                 e1 e2 -> liftA2 LesserEqExpr                 (f e1) (f e2)
    BoolAndExpr                  e1 e2 -> liftA2 BoolAndExpr                  (f e1) (f e2)
    BoolOrExpr                   e1 e2 -> liftA2 BoolOrExpr                   (f e1) (f e2)
    RangeInclusiveExpr           e1 e2 -> liftA2 RangeInclusiveExpr           (f e1) (f e2)
    RangeExclusiveExpr           e1 e2 -> liftA2 RangeExclusiveExpr           (f e1) (f e2)
    AssignmentExpr               e1 e2 -> liftA2 AssignmentExpr               (f e1) (f e2)
    AdditionAssignmentExpr       e1 e2 -> liftA2 AdditionAssignmentExpr       (f e1) (f e2)
    SubtractionAssignmentExpr    e1 e2 -> liftA2 SubtractionAssignmentExpr    (f e1) (f e2)
    MultiplicationAssignmentExpr e1 e2 -> liftA2 MultiplicationAssignmentExpr (f e1) (f e2)
    DivisionAssignmentExpr       e1 e2 -> liftA2 DivisionAssignmentExpr       (f e1) (f e2)
    ModuloAssignmentExpr         e1 e2 -> liftA2 ModuloAssignmentExpr         (f e1) (f e2)
    ExponentiationAssignmentExpr e1 e2 -> liftA2 ExponentiationAssignmentExpr (f e1) (f e2)
    e                                  -> pure e



--------------------------------------------------------------------------------
-- Order-dependent declarations

-- Due to lenses, some declarations must be put at the end of the file, *after*
-- the corresponding lens declaration.

instance Annotation Expression Resolved where
  type Annotated Expression Resolved = TypedExpression
  within = exprValue
