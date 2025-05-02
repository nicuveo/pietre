module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Control.Lens
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.HashMap.Strict                    qualified as M
import Data.Tuple

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Monad


--------------------------------------------------------------------------------
-- Analysis

type Symbols = HashMap Identifier (Declaration Resolved)

analyzeModule
  :: HashMap ModuleName Symbols
  -> ModuleName
  -> Module
  -> ([Diagnostic], Maybe Symbols)
analyzeModule context moduleName Module {..} = swap $ runWriter $ runMaybeT do
  scope <- initialScope context _modImports
  let (diagnostics, symbols) = unzip $ map
        (runAnalysis moduleName scope . analyze)
        _modDeclarations
  tell $ concat diagnostics
  symbolsList <- hoistMaybe $ sequence symbols
  pure $ M.fromList symbolsList


--------------------------------------------------------------------------------
-- Internals

initialScope
  :: HashMap ModuleName Symbols
  -> [Import]
  -> MaybeT (Writer [Diagnostic]) Scope
initialScope = undefined


class Analyzable t where
  type Analyzed t
  analyze :: t -> AnalyzeM (Analyzed t)

instance Analyzable a => Analyzable (WithLocation a) where
  type Analyzed (WithLocation a) = Analyzed a
  analyze WithLocation {..} =
    local (contextLocation .~ _location) $
    analyze _located

instance Analyzable (Declaration Parsed) where
  type Analyzed (Declaration Parsed) = (Identifier, Declaration Resolved)
  analyze = \case
    TypeAliasDecl info -> (undefined,) . TypeAliasDecl <$> analyze info
    EnumDecl      info -> (undefined,) . EnumDecl      <$> analyze info
    StructDecl    info -> (undefined,) . StructDecl    <$> analyze info
    ConstDecl     info -> (undefined,) . ConstDecl     <$> analyze info
    FunctionDecl  info -> (undefined,) . FunctionDecl  <$> analyze info

instance Analyzable (Statement Parsed) where
  type Analyzed (Statement Parsed) = Statement Resolved
  analyze = \case
    IfStmt         info    -> IfStmt         <$> analyze info
    ForStmt        info    -> ForStmt        <$> analyze info
    WhileStmt      info    -> WhileStmt      <$> analyze info
    LetStmt        info    -> LetStmt        <$> analyze info
    ExpressionStmt expr    -> ExpressionStmt <$> analyze expr
    ReturnStmt     _retval -> undefined
    ContinueStmt           -> undefined
    BreakStmt              -> undefined

instance Analyzable (Expression Parsed) where
  type Analyzed (Expression Parsed) = Expression Resolved
  analyze = \case
    PathExpr                     _path             -> undefined
    FieldAccessExpr              _expr _identifier -> undefined
    CallExpr                     _path _args       -> undefined
    ArrayExpr                    _array            -> undefined
    IndexExpr                    _expr _index      -> undefined
    StructExpr                   _path _fields     -> undefined
    BoolLiteralExpr              _b                -> undefined
    IntLiteralExpr               _i                -> undefined
    CharLiteralExpr              _c                -> undefined
    StringLiteralExpr            _t                -> undefined
    ReferenceExpr                _path             -> undefined
    NegationExpr                 _expr             -> undefined
    CastExpr                     _expr  _path      -> undefined
    AdditionExpr                 _exprL _exprR     -> undefined
    SubtractionExpr              _exprL _exprR     -> undefined
    MultiplicationExpr           _exprL _exprR     -> undefined
    DivisionExpr                 _exprL _exprR     -> undefined
    ModuloExpr                   _exprL _exprR     -> undefined
    ExponentiationExpr           _exprL _exprR     -> undefined
    EqualityExpr                 _exprL _exprR     -> undefined
    DifferenceExpr               _exprL _exprR     -> undefined
    GreaterExpr                  _exprL _exprR     -> undefined
    LesserExpr                   _exprL _exprR     -> undefined
    GreaterEqExpr                _exprL _exprR     -> undefined
    LesserEqExpr                 _exprL _exprR     -> undefined
    BoolAndExpr                  _exprL _exprR     -> undefined
    BoolOrExpr                   _exprL _exprR     -> undefined
    RangeInclusiveExpr           _exprL _exprR     -> undefined
    RangeExclusiveExpr           _exprL _exprR     -> undefined
    AssignmentExpr               _exprL _exprR     -> undefined
    AdditionAssignmentExpr       _exprL _exprR     -> undefined
    SubtractionAssignmentExpr    _exprL _exprR     -> undefined
    MultiplicationAssignmentExpr _exprL _exprR     -> undefined
    DivisionAssignmentExpr       _exprL _exprR     -> undefined
    ModuloAssignmentExpr         _exprL _exprR     -> undefined
    ExponentiationAssignmentExpr _exprL _exprR     -> undefined


instance Analyzable (TypeAliasInfo Parsed) where
  type Analyzed (TypeAliasInfo Parsed) = TypeAliasInfo Resolved
  analyze = undefined

instance Analyzable (EnumInfo Parsed) where
  type Analyzed (EnumInfo Parsed) = EnumInfo Resolved
  analyze = undefined

instance Analyzable (StructInfo Parsed) where
  type Analyzed (StructInfo Parsed) = StructInfo Resolved
  analyze = undefined

instance Analyzable (ConstInfo Parsed) where
  type Analyzed (ConstInfo Parsed) = ConstInfo Resolved
  analyze = undefined

instance Analyzable (FunctionInfo Parsed) where
  type Analyzed (FunctionInfo Parsed) = FunctionInfo Resolved
  analyze = undefined

instance Analyzable (FunctionArgType Parsed) where
  type Analyzed (FunctionArgType Parsed) = FunctionArgType Resolved
  analyze = undefined

instance Analyzable (IfInfo Parsed) where
  type Analyzed (IfInfo Parsed) = IfInfo Resolved
  analyze = undefined

instance Analyzable (ElseInfo Parsed) where
  type Analyzed (ElseInfo Parsed) = ElseInfo Resolved
  analyze = undefined

instance Analyzable (ForInfo Parsed) where
  type Analyzed (ForInfo Parsed) = ForInfo Resolved
  analyze = undefined

instance Analyzable (WhileInfo Parsed) where
  type Analyzed (WhileInfo Parsed) = WhileInfo Resolved
  analyze = undefined

instance Analyzable (LetInfo Parsed) where
  type Analyzed (LetInfo Parsed) = LetInfo Resolved
  analyze = undefined
