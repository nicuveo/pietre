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


--------------------------------------------------------------------------------
-- AST Phase

class ASTRepresentation p where
  type NameType p :: Type

type ShowConstraints p =
  ( Show (NameType p)
  )


--------------------------------------------------------------------------------
-- Generic AST

data Definition p
  = TypeAliasDef (TypeAliasInfo p)
  | EnumDef      EnumInfo
  | StructDef    (StructInfo    p)
  | ConstDef     (ConstInfo     p)
  | FunctionDef  (FunctionInfo  p)

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
  , _constExpr :: WithLocation (Expression p)
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
  = IfStmt         (IfInfo    p)
  | ForStmt        (ForInfo   p)
  | WhileStmt      (WhileInfo p)
  | LetStmt        (LetInfo   p)
  | ReturnStmt     (Maybe (WithLocation (Expression p)))
  | ContinueStmt
  | BreakStmt
  | ExpressionStmt (WithLocation (Expression p))

deriving instance ShowConstraints p => Show (Statement p)

type Block p = [WithLocation (Statement p)]


data IfInfo p = IfInfo
  { _ifExpr :: WithLocation (Expression p)
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
  , _forRangeExpr    :: WithLocation (Expression p)
  , _forBody         :: Block p
  }

deriving instance ShowConstraints p => Show (ForInfo p)


data WhileInfo p = WhileInfo
  { _whileExpr :: WithLocation (Expression p)
  , _whileBody :: Block p
  }

deriving instance ShowConstraints p => Show (WhileInfo p)


data LetInfo p = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (PathInfo p)
  , _letExpr :: WithLocation (Expression p)
  }

deriving instance ShowConstraints p => Show (LetInfo p)


data Expression p
  = PathExpr                     (PathInfo p)
  | FieldAccessExpr              (WithLocation (Expression p)) Identifier
  | CallExpr                     (PathInfo p) [WithLocation (Expression p)]
  | ArrayExpr                    [WithLocation (Expression p)]
  | IndexExpr                    (WithLocation (Expression p)) (WithLocation (Expression p))
  | StructExpr                   (PathInfo p) (NonEmpty (Identifier, WithLocation (Expression p)))
  | BoolLiteralExpr              Bool
  | IntLiteralExpr               Int
  | CharLiteralExpr              Char
  | StringLiteralExpr            Text
  | ReferenceExpr                (PathInfo p)
  | IntNegationExpr              (WithLocation (Expression p))
  | BoolNegationExpr             (WithLocation (Expression p))
  | AdditionExpr                 (WithLocation (Expression p)) (WithLocation (Expression p))
  | SubtractionExpr              (WithLocation (Expression p)) (WithLocation (Expression p))
  | MultiplicationExpr           (WithLocation (Expression p)) (WithLocation (Expression p))
  | DivisionExpr                 (WithLocation (Expression p)) (WithLocation (Expression p))
  | ModuloExpr                   (WithLocation (Expression p)) (WithLocation (Expression p))
  | ExponentiationExpr           (WithLocation (Expression p)) (WithLocation (Expression p))
  | EqualityExpr                 (WithLocation (Expression p)) (WithLocation (Expression p))
  | DifferenceExpr               (WithLocation (Expression p)) (WithLocation (Expression p))
  | GreaterExpr                  (WithLocation (Expression p)) (WithLocation (Expression p))
  | LesserExpr                   (WithLocation (Expression p)) (WithLocation (Expression p))
  | GreaterEqExpr                (WithLocation (Expression p)) (WithLocation (Expression p))
  | LesserEqExpr                 (WithLocation (Expression p)) (WithLocation (Expression p))
  | BoolAndExpr                  (WithLocation (Expression p)) (WithLocation (Expression p))
  | BoolOrExpr                   (WithLocation (Expression p)) (WithLocation (Expression p))
  | CastExpr                     (WithLocation (Expression p)) (PathInfo p)
  | RangeInclusiveExpr           (WithLocation (Expression p)) (WithLocation (Expression p))
  | RangeExclusiveExpr           (WithLocation (Expression p)) (WithLocation (Expression p))
  | AssignmentExpr               (WithLocation (Expression p)) (WithLocation (Expression p))
  | AdditionAssignmentExpr       (WithLocation (Expression p)) (WithLocation (Expression p))
  | SubtractionAssignmentExpr    (WithLocation (Expression p)) (WithLocation (Expression p))
  | MultiplicationAssignmentExpr (WithLocation (Expression p)) (WithLocation (Expression p))
  | DivisionAssignmentExpr       (WithLocation (Expression p)) (WithLocation (Expression p))
  | ModuloAssignmentExpr         (WithLocation (Expression p)) (WithLocation (Expression p))
  | ExponentiationAssignmentExpr (WithLocation (Expression p)) (WithLocation (Expression p))

deriving instance ShowConstraints p => Show (Expression p)
deriving instance Eq (WithLocation Expression Resolved) => Eq (Expression Resolved)


data PathInfo p = PathInfo
  { _pathName   :: NameType p
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
