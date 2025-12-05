{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Representations.AST.Common where

import "this" Prelude

import Control.Lens
import Data.Kind

import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location


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

data CommonDefinition p
  = TypeAliasDef (CommonTypeAliasInfo p)
  | EnumDef      EnumInfo
  | StructDef    (CommonStructInfo p)
  | ConstDef     (CommonConstInfo p)
  | FunctionDef  (CommonFunctionInfo p)

deriving instance ShowConstraints p => Show (CommonDefinition p)


data CommonTypeAliasInfo p = TypeAliasInfo
  { _aliasName   :: Identifier
  , _aliasParams :: [Identifier]
  , _aliasValue  :: CommonPathInfo p
  }

deriving instance ShowConstraints p => Show (CommonTypeAliasInfo p)


data EnumInfo = EnumInfo
  { _enumName   :: Identifier
  , _enumValues :: [Identifier]
  } deriving Show


data CommonStructInfo p = StructInfo
  { _structName   :: Identifier
  , _structParams :: [Identifier]
  , _structValues :: NonEmpty (Identifier, CommonPathInfo p)
  }

deriving instance ShowConstraints p => Show (CommonStructInfo p)


data CommonConstInfo p = ConstInfo
  { _constName :: Identifier
  , _constType :: CommonPathInfo p
  , _constExpr :: ExpressionType p
  }

deriving instance ShowConstraints p => Show (CommonConstInfo p)


data CommonFunctionInfo p = FunctionInfo
  { _funName :: Identifier
  , _funType :: CommonFunctionType p
  , _funBody :: CommonBlock p
  }

deriving instance ShowConstraints p => Show (CommonFunctionInfo p)

isGeneric :: CommonFunctionInfo p -> Bool
isGeneric = not . null . _funParams . _funType


data CommonFunctionType p = FunctionType
  { _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, CommonFunctionArgType p)]
  , _funReturn :: Maybe (CommonPathInfo p)
  } deriving Generic

deriving instance ShowConstraints p => Show (CommonFunctionType p)


data CommonFunctionArgType p
  = ByValue     (CommonPathInfo p)
  | ByReference (CommonPathInfo p)
  deriving Generic

functionArgType :: CommonFunctionArgType p -> CommonPathInfo p
functionArgType = \case
  ByValue     p -> p
  ByReference p -> p

deriving instance ShowConstraints p => Show (CommonFunctionArgType p)


data CommonStatement p
  = IfStmt         (CommonIfInfo    p)
  | ForStmt        (ForInfoType     p)
  | WhileStmt      (CommonWhileInfo p)
  | LetStmt        (LetInfoType     p)
  | ReturnStmt     (Maybe (ExpressionType p))
  | ContinueStmt
  | BreakStmt
  | ExpressionStmt (ExpressionType p)

deriving instance ShowConstraints p => Show (CommonStatement p)

type CommonBlock p = [WithLocation (CommonStatement p)]


data CommonIfInfo p = IfInfo
  { _ifExpr :: ExpressionType p
  , _ifBody :: CommonBlock p
  , _ifElse :: Maybe (CommonElseInfo p)
  }

deriving instance ShowConstraints p => Show (CommonIfInfo p)


data CommonElseInfo p
  = ElseIf    (CommonIfInfo p)
  | ElseBlock (CommonBlock p)

deriving instance ShowConstraints p => Show (CommonElseInfo p)


data CommonForInfo p = ForInfo
  { _forVariableName :: Identifier
  , _forRangeExpr    :: ExpressionType p
  , _forBody         :: CommonBlock p
  }

deriving instance ShowConstraints p => Show (CommonForInfo p)


data CommonWhileInfo p = WhileInfo
  { _whileExpr :: ExpressionType p
  , _whileBody :: CommonBlock p
  }

deriving instance ShowConstraints p => Show (CommonWhileInfo p)


data CommonLetInfo p = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (CommonPathInfo p)
  , _letExpr :: ExpressionType p
  }

deriving instance ShowConstraints p => Show (CommonLetInfo p)


data CommonExpression p
  = PathExpr                     (CommonPathInfo p)
  | FieldAccessExpr              (ExpressionType p) Identifier
  | CallExpr                     (CommonPathInfo p) [ExpressionType p]
  | ArrayExpr                    [ExpressionType p]
  | IndexExpr                    (ExpressionType p) (ExpressionType p)
  | StructExpr                   (CommonPathInfo p) (NonEmpty (Identifier, ExpressionType p))
  | BoolLiteralExpr              Bool
  | IntLiteralExpr               Int
  | CharLiteralExpr              Char
  | StringLiteralExpr            Text
  | ReferenceExpr                (CommonPathInfo p)
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
  | CastExpr                     (ExpressionType p) (CommonPathInfo p)
  | RangeInclusiveExpr           (ExpressionType p) (ExpressionType p)
  | RangeExclusiveExpr           (ExpressionType p) (ExpressionType p)
  | AssignmentExpr               (ExpressionType p) (ExpressionType p)
  | AdditionAssignmentExpr       (ExpressionType p) (ExpressionType p)
  | SubtractionAssignmentExpr    (ExpressionType p) (ExpressionType p)
  | MultiplicationAssignmentExpr (ExpressionType p) (ExpressionType p)
  | DivisionAssignmentExpr       (ExpressionType p) (ExpressionType p)
  | ModuloAssignmentExpr         (ExpressionType p) (ExpressionType p)
  | ExponentiationAssignmentExpr (ExpressionType p) (ExpressionType p)

deriving instance ShowConstraints p => Show (CommonExpression p)


data CommonPathInfo p = PathInfo
  { _pathBase   :: PathBodyType p
  , _pathParams :: [CommonPathInfo p]
  } deriving (Generic)

deriving instance ShowConstraints p => Show (CommonPathInfo p)


--------------------------------------------------------------------------------
-- Lenses

makeLenses ''CommonConstInfo
makeLenses ''CommonForInfo
makeLenses ''CommonFunctionInfo
makeLenses ''CommonIfInfo
makeLenses ''CommonLetInfo
makeLenses ''CommonPathInfo
makeLenses ''CommonStructInfo
makeLenses ''CommonTypeAliasInfo
makeLenses ''CommonWhileInfo
makeLenses ''EnumInfo

makePrisms ''CommonDefinition
makePrisms ''CommonElseInfo
makePrisms ''CommonExpression
makePrisms ''CommonFunctionArgType
makePrisms ''CommonStatement
