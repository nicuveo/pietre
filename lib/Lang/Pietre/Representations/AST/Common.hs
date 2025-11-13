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


--------------------------------------------------------------------------------
-- AST Phase

data ASTPhase = Parsed | Resolved | Validated
  deriving (Show, Eq, Ord, Enum, Bounded)

class ASTRepresentation (p :: ASTPhase) where
  type PathBodyType   p :: Type
  type ExpressionType p :: Type
  type ForInfoType    p :: Type
  type LetInfoType    p :: Type

type ShowConstraints p =
  ( Show (PathBodyType   p)
  , Show (ExpressionType p)
  , Show (ForInfoType    p)
  , Show (LetInfoType    p)
  )


--------------------------------------------------------------------------------
-- Common AST definitions

data Definition p
  = TypeAliasDef (TypeAliasInfo p)
  | EnumDef      EnumInfo
  | StructDef    (StructInfo p)
  | ConstDef     (ConstInfo p)
  | FunctionDef  (FunctionInfo p)

deriving instance ShowConstraints p => Show (Definition p)


data TypeAliasInfo p = TypeAliasInfo
  { _aliasName   :: Identifier
  , _aliasParams :: [Identifier]
  , _aliasValue  :: PathInfo p
  }

deriving instance ShowConstraints p => Show (TypeAliasInfo p)


data EnumInfo = EnumInfo
  { _enumName   :: Identifier
  , _enumValues :: [Identifier]
  }

deriving instance ShowConstraints p => Show (EnumInfo p)


data StructInfo p = StructInfo
  { _structName   :: Identifier
  , _structParams :: [Identifier]
  , _structValues :: NonEmpty (Identifier, PathInfo p)
  }

deriving instance ShowConstraints p => Show (StructInfo p)


data ConstInfo p = ConstInfo
  { _constName :: Identifier
  , _constType :: PathInfo p
  , _constExpr :: ExpressionType p
  }

deriving instance ShowConstraints p => Show (ConstInfo p)


data FunctionInfo p = FunctionInfo
  { _funName :: Identifier
  , _funType :: FunctionType p
  , _funBody :: Block p
  }

deriving instance ShowConstraints p => Show (FunctionInfo p)


data FunctionType p = FunctionType
  { _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, FunctionArgType p)]
  , _funReturn :: Maybe (PathInfo p)
  } deriving Generic

deriving instance Eq  (FunctionType Resolved)
deriving instance Ord (FunctionType Resolved)
instance Hashable (FunctionType Resolved)

deriving instance ShowConstraints p => Show (FunctionType p)


data FunctionArgType p
  = ByValue     (PathInfo p)
  | ByReference (PathInfo p)
  deriving Generic

deriving instance Eq  (FunctionArgType Resolved)
deriving instance Ord (FunctionArgType Resolved)
instance Hashable (FunctionArgType Resolved)

functionArgType :: FunctionArgType p -> PathInfo p
functionArgType = \case
  ByValue     p -> p
  ByReference p -> p

deriving instance ShowConstraints p => Show (FunctionArgType p)


data Statement p
  = IfStmt         (IfInfo      p)
  | ForStmt        (ForInfoType p)
  | WhileStmt      (WhileInfo   p)
  | LetStmt        (LetInfoType p)
  | ReturnStmt     (Maybe (ExpressionType p))
  | ContinueStmt
  | BreakStmt
  | ExpressionStmt (ExpressionType p)

deriving instance ShowConstraints p => Show (Statement p)

type Block p = [WithLocation (Statement p)]


data IfInfo p = IfInfo
  { _ifExpr :: ExpressionType p
  , _ifBody :: Block p
  , _ifElse :: Maybe (ElseInfo p)
  }

deriving instance ShowConstraints p => Show (IfInfo p)


data ElseInfo p
  = ElseIf    (IfInfo p)
  | ElseBlock (Block p)

deriving instance ShowConstraints p => Show (ElseInfo p)


data ForInfo p = ForInfo
  { _forVariableName :: Identifier
  , _forRangeExpr    :: ExpressionType p
  , _forBody         :: Block p
  }

deriving instance ShowConstraints p => Show (ForInfo p)


data WhileInfo p = WhileInfo
  { _whileExpr :: ExpressionType p
  , _whileBody :: Block p
  }

deriving instance ShowConstraints p => Show (WhileInfo p)


data LetInfo p = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (PathInfo p)
  , _letExpr :: ExpressionType p
  }

deriving instance ShowConstraints p => Show (LetInfo p)


data Expression p
  = PathExpr                     (PathInfo p)
  | FieldAccessExpr              (ExpressionType p) Identifier
  | CallExpr                     (PathInfo p) [ExpressionType p]
  | ArrayExpr                    [ExpressionType p]
  | IndexExpr                    (ExpressionType p) (ExpressionType p)
  | StructExpr                   (PathInfo p) (NonEmpty (Identifier, ExpressionType p))
  | BoolLiteralExpr              Bool
  | IntLiteralExpr               Int
  | CharLiteralExpr              Char
  | StringLiteralExpr            Text
  | ReferenceExpr                (PathInfo p)
  | IntNegationExpr              (ExpressionType p)
  | BoolNegationExpr             (ExpressionType p)
  | AdditionExpr                 (ExpressionType p) (ExpressionType p)
  | SubtractionExpr              (ExpressionType p) (ExpressionType p)
  | MultiplicationExpr           (ExpressionType p) (ExpressionType p)
  | DivisionExpr                 (ExpressionType p) (ExpressionType p)
  | ModuloExpr                   (ExpressionType p) (ExpressionType p)
  | ExponentiationExpr           (ExpressionType p) (ExpressionType p)
  | EqualityExpr                 (ExpressionType p) (ExpressionType p)
  | DifferenceExpr               (ExpressionType p) (ExpressionType p)
  | GreaterExpr                  (ExpressionType p) (ExpressionType p)
  | LesserExpr                   (ExpressionType p) (ExpressionType p)
  | GreaterEqExpr                (ExpressionType p) (ExpressionType p)
  | LesserEqExpr                 (ExpressionType p) (ExpressionType p)
  | BoolAndExpr                  (ExpressionType p) (ExpressionType p)
  | BoolOrExpr                   (ExpressionType p) (ExpressionType p)
  | CastExpr                     (ExpressionType p) (PathInfo p)
  | RangeInclusiveExpr           (ExpressionType p) (ExpressionType p)
  | RangeExclusiveExpr           (ExpressionType p) (ExpressionType p)
  | AssignmentExpr               (ExpressionType p) (ExpressionType p)
  | AdditionAssignmentExpr       (ExpressionType p) (ExpressionType p)
  | SubtractionAssignmentExpr    (ExpressionType p) (ExpressionType p)
  | MultiplicationAssignmentExpr (ExpressionType p) (ExpressionType p)
  | DivisionAssignmentExpr       (ExpressionType p) (ExpressionType p)
  | ModuloAssignmentExpr         (ExpressionType p) (ExpressionType p)
  | ExponentiationAssignmentExpr (ExpressionType p) (ExpressionType p)

deriving instance ShowConstraints p => Show (Expression p)
deriving instance Eq (WithLocation Expression Resolved) => Eq (Expression Resolved)


data PathInfo p = PathInfo
  { _pathName   :: PathBodyType p
  , _pathParams :: [PathInfo p]
  } deriving (Generic)

deriving instance ShowConstraints p => Show (PathInfo p)
deriving instance Eq  (PathInfo Resolved)
deriving instance Ord (PathInfo Resolved)
instance Hashable (PathInfo Resolved)


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

instance Plated (Expression p) where
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
