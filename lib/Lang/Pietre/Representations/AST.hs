{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.AST where

import "this" Prelude

import Control.Applicative                  (liftA3)
import Control.Lens
import Data.Kind
import Data.List.NonEmpty                   qualified as NE
import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens


--------------------------------------------------------------------------------
-- AST Phase

data ASTPhase = Parsed

class
  ( Show (XTypeAlias                p)
  , Show (XEnum                     p)
  , Show (XStructDecl               p)
  , Show (XConst                    p)
  , Show (XFunction                 p)
  , Show (XIf                       p)
  , Show (XFor                      p)
  , Show (XWhile                    p)
  , Show (XLet                      p)
  , Show (XReturn                   p)
  , Show (XContinue                 p)
  , Show (XBreak                    p)
  , Show (XEpression                p)
  , Show (XPath                     p)
  , Show (XFieldAccess              p)
  , Show (XCall                     p)
  , Show (XArray                    p)
  , Show (XIndex                    p)
  , Show (XStructExpr               p)
  , Show (XLiteral                  p)
  , Show (XReference                p)
  , Show (XNegation                 p)
  , Show (XAddition                 p)
  , Show (XSubtraction              p)
  , Show (XMultiplication           p)
  , Show (XDivision                 p)
  , Show (XModulo                   p)
  , Show (XExponentiation           p)
  , Show (XEquality                 p)
  , Show (XDifference               p)
  , Show (XGreater                  p)
  , Show (XLesser                   p)
  , Show (XGreaterEq                p)
  , Show (XLesserEq                 p)
  , Show (XBoolAnd                  p)
  , Show (XBoolOr                   p)
  , Show (XCast                     p)
  , Show (XInclusiveRange           p)
  , Show (XExclusiveRange           p)
  , Show (XAssignment               p)
  , Show (XAdditionAssignment       p)
  , Show (XSubtractionAssignment    p)
  , Show (XMultiplicationAssignment p)
  , Show (XDivisionAssignment       p)
  , Show (XModuloAssignment         p)
  , Show (XExponentiationAssignment p)
  ) => ASTRepresentation (p :: ASTPhase) where
  type XTypeAlias                p :: Type
  type XEnum                     p :: Type
  type XStructDecl               p :: Type
  type XConst                    p :: Type
  type XFunction                 p :: Type
  type XIf                       p :: Type
  type XFor                      p :: Type
  type XWhile                    p :: Type
  type XLet                      p :: Type
  type XReturn                   p :: Type
  type XContinue                 p :: Type
  type XBreak                    p :: Type
  type XEpression                p :: Type
  type XPath                     p :: Type
  type XFieldAccess              p :: Type
  type XCall                     p :: Type
  type XArray                    p :: Type
  type XIndex                    p :: Type
  type XStructExpr               p :: Type
  type XLiteral                  p :: Type
  type XReference                p :: Type
  type XNegation                 p :: Type
  type XAddition                 p :: Type
  type XSubtraction              p :: Type
  type XMultiplication           p :: Type
  type XDivision                 p :: Type
  type XModulo                   p :: Type
  type XExponentiation           p :: Type
  type XEquality                 p :: Type
  type XDifference               p :: Type
  type XGreater                  p :: Type
  type XLesser                   p :: Type
  type XGreaterEq                p :: Type
  type XLesserEq                 p :: Type
  type XBoolAnd                  p :: Type
  type XBoolOr                   p :: Type
  type XCast                     p :: Type
  type XInclusiveRange           p :: Type
  type XExclusiveRange           p :: Type
  type XAssignment               p :: Type
  type XAdditionAssignment       p :: Type
  type XSubtractionAssignment    p :: Type
  type XMultiplicationAssignment p :: Type
  type XDivisionAssignment       p :: Type
  type XModuloAssignment         p :: Type
  type XExponentiationAssignment p :: Type

instance ASTRepresentation Parsed where
  type XTypeAlias                Parsed = Location
  type XEnum                     Parsed = Location
  type XStructDecl               Parsed = Location
  type XConst                    Parsed = Location
  type XFunction                 Parsed = Location
  type XIf                       Parsed = Location
  type XFor                      Parsed = Location
  type XWhile                    Parsed = Location
  type XLet                      Parsed = Location
  type XReturn                   Parsed = Location
  type XContinue                 Parsed = Location
  type XBreak                    Parsed = Location
  type XEpression                Parsed = Location
  type XPath                     Parsed = Location
  type XFieldAccess              Parsed = Location
  type XCall                     Parsed = Location
  type XArray                    Parsed = Location
  type XIndex                    Parsed = Location
  type XStructExpr               Parsed = Location
  type XLiteral                  Parsed = Location
  type XReference                Parsed = Location
  type XNegation                 Parsed = Location
  type XAddition                 Parsed = Location
  type XSubtraction              Parsed = Location
  type XMultiplication           Parsed = Location
  type XDivision                 Parsed = Location
  type XModulo                   Parsed = Location
  type XExponentiation           Parsed = Location
  type XEquality                 Parsed = Location
  type XDifference               Parsed = Location
  type XGreater                  Parsed = Location
  type XLesser                   Parsed = Location
  type XGreaterEq                Parsed = Location
  type XLesserEq                 Parsed = Location
  type XBoolAnd                  Parsed = Location
  type XBoolOr                   Parsed = Location
  type XCast                     Parsed = Location
  type XInclusiveRange           Parsed = Location
  type XExclusiveRange           Parsed = Location
  type XAssignment               Parsed = Location
  type XAdditionAssignment       Parsed = Location
  type XSubtractionAssignment    Parsed = Location
  type XMultiplicationAssignment Parsed = Location
  type XDivisionAssignment       Parsed = Location
  type XModuloAssignment         Parsed = Location
  type XExponentiationAssignment Parsed = Location


--------------------------------------------------------------------------------
-- AST

data Module (p :: ASTPhase) = Module
  { _modImports      :: [Import]
  , _modDeclarations :: [Declaration p]
  }

deriving instance ASTRepresentation p => Show (Module p)

instance Semigroup (Module p) where
  Module imports1 decls1 <> Module imports2 decls2 =
    Module (imports1 <> imports2) (decls1 <> decls2)

instance Monoid (Module p) where
  mempty = Module [] []


data Import = Import
  { _importPath :: NonEmpty Identifier
  , _importType :: ImportType
  }
  deriving Show


data ImportType
  = Qualified  (Maybe Identifier)
  | Specific   (NonEmpty Identifier)
  | Exhaustive
  deriving Show


data Declaration (p :: ASTPhase)
  = TypeAliasDecl (XTypeAlias  p) (TypeAliasInfo p)
  | EnumDecl      (XEnum       p) (EnumInfo      p)
  | StructDecl    (XStructDecl p) (StructInfo    p)
  | ConstDecl     (XConst      p) (ConstInfo     p)
  | FunctionDecl  (XFunction   p) (FunctionInfo  p)

deriving instance ASTRepresentation p => Show (Declaration p)


data TypeAliasInfo (p :: ASTPhase) = TypeAliasInfo
  { _aliasName   :: Identifier
  , _aliasParams :: [Identifier]
  , _aliasValue  :: TypeExpr p
  }

deriving instance ASTRepresentation p => Show (TypeAliasInfo p)


data EnumInfo (p :: ASTPhase) = EnumInfo
  { _enumName   :: Identifier
  , _enumValues :: [Identifier]
  }

deriving instance ASTRepresentation p => Show (EnumInfo p)


data StructInfo (p :: ASTPhase) = StructInfo
  { _structName   :: Identifier
  , _structParams :: [Identifier]
  , _structValues :: NonEmpty (Identifier, TypeExpr p)
  }

deriving instance ASTRepresentation p => Show (StructInfo p)


data ConstInfo (p :: ASTPhase) = ConstInfo
  { _constName :: Identifier
  , _constType :: TypeExpr p
  , _constExpr :: Expression p
  }

deriving instance ASTRepresentation p => Show (ConstInfo p)


data FunctionInfo (p :: ASTPhase) = FunctionInfo
  { _funName   :: Identifier
  , _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, FunctionArgType p)]
  , _funType   :: Maybe (TypeExpr p)
  , _funBody   :: [Statement p]
  }

deriving instance ASTRepresentation p => Show (FunctionInfo p)


data FunctionArgType (p :: ASTPhase)
  = ByValue     (TypeExpr p)
  | ByReference (TypeExpr p)

deriving instance ASTRepresentation p => Show (FunctionArgType p)


data Statement (p :: ASTPhase)
  = IfStmt         (XIf        p) (IfInfo    p)
  | ForStmt        (XFor       p) (ForInfo   p)
  | WhileStmt      (XWhile     p) (WhileInfo p)
  | LetStmt        (XLet       p) (LetInfo   p)
  | ReturnStmt     (XReturn    p) (Maybe (Expression p))
  | ContinueStmt   (XContinue  p)
  | BreakStmt      (XBreak     p)
  | ExpressionStmt (XEpression p) (Expression p)

deriving instance ASTRepresentation p => Show (Statement p)


data IfInfo (p :: ASTPhase) = IfInfo
  { _ifExpr :: Expression p
  , _ifBody :: [Statement p]
  , _ifElse :: Maybe (ElseInfo p)
  }

deriving instance ASTRepresentation p => Show (IfInfo p)


data ElseInfo (p :: ASTPhase)
  = ElseIf    (IfInfo p)
  | ElseBlock [Statement p]

deriving instance ASTRepresentation p => Show (ElseInfo p)


data ForInfo (p :: ASTPhase) = ForInfo
  { _forVariableName :: Identifier
  , _forRangeExpr    :: Expression p
  , _forBody         :: [Statement p]
  }

deriving instance ASTRepresentation p => Show (ForInfo p)


data WhileInfo (p :: ASTPhase) = WhileInfo
  { _whileExpr :: Expression p
  , _whileBody :: [Statement p]
  }

deriving instance ASTRepresentation p => Show (WhileInfo p)


data LetInfo (p :: ASTPhase) = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (TypeExpr p)
  , _letExpr :: Expression p
  }

deriving instance ASTRepresentation p => Show (LetInfo p)


data Expression (p :: ASTPhase)
  = PathExpr                     (XPath                     p) (PathInfo p)
  | FieldAccessExpr              (XFieldAccess              p) (Expression p) Identifier
  | CallExpr                     (XCall                     p) (PathInfo p)   [Expression p]
  | ArrayExpr                    (XArray                    p) [Expression p]
  | IndexExpr                    (XIndex                    p) (Expression p) (Expression p)
  | StructExpr                   (XStructExpr               p) (PathInfo p)   [(Identifier, Expression p)]
  | BoolLiteralExpr              (XLiteral                  p) Bool
  | IntLiteralExpr               (XLiteral                  p) Int
  | CharLiteralExpr              (XLiteral                  p) Char
  | StringLiteralExpr            (XLiteral                  p) Text
  | ReferenceExpr                (XReference                p) (PathInfo p)
  | NegationExpr                 (XNegation                 p) (Expression p)
  | AdditionExpr                 (XAddition                 p) (Expression p) (Expression p)
  | SubtractionExpr              (XSubtraction              p) (Expression p) (Expression p)
  | MultiplicationExpr           (XMultiplication           p) (Expression p) (Expression p)
  | DivisionExpr                 (XDivision                 p) (Expression p) (Expression p)
  | ModuloExpr                   (XModulo                   p) (Expression p) (Expression p)
  | ExponentiationExpr           (XExponentiation           p) (Expression p) (Expression p)
  | EqualityExpr                 (XEquality                 p) (Expression p) (Expression p)
  | DifferenceExpr               (XDifference               p) (Expression p) (Expression p)
  | GreaterExpr                  (XGreater                  p) (Expression p) (Expression p)
  | LesserExpr                   (XLesser                   p) (Expression p) (Expression p)
  | GreaterEqExpr                (XGreaterEq                p) (Expression p) (Expression p)
  | LesserEqExpr                 (XLesserEq                 p) (Expression p) (Expression p)
  | BoolAndExpr                  (XBoolAnd                  p) (Expression p) (Expression p)
  | BoolOrExpr                   (XBoolOr                   p) (Expression p) (Expression p)
  | CastExpr                     (XCast                     p) (Expression p) (TypeExpr p)
  | RangeInclusiveExpr           (XInclusiveRange           p) (Expression p) (Expression p)
  | RangeExclusiveExpr           (XExclusiveRange           p) (Expression p) (Expression p)
  | AssignmentExpr               (XAssignment               p) (Expression p) (Expression p)
  | AdditionAssignmentExpr       (XAdditionAssignment       p) (Expression p) (Expression p)
  | SubtractionAssignmentExpr    (XSubtractionAssignment    p) (Expression p) (Expression p)
  | MultiplicationAssignmentExpr (XMultiplicationAssignment p) (Expression p) (Expression p)
  | DivisionAssignmentExpr       (XDivisionAssignment       p) (Expression p) (Expression p)
  | ModuloAssignmentExpr         (XModuloAssignment         p) (Expression p) (Expression p)
  | ExponentiationAssignmentExpr (XExponentiationAssignment p) (Expression p) (Expression p)

deriving instance ASTRepresentation p => Show (Expression p)


data PathInfo (p :: ASTPhase) = PathInfo
  { _pathName   :: NonEmpty Identifier
  , _pathParams :: [TypeExpr p]
  }

type TypeExpr (p :: ASTPhase) = PathInfo p

deriving instance ASTRepresentation p => Show (TypeExpr p)


--------------------------------------------------------------------------------
-- Pretty print

instance Pretty (Module p) where
  pretty Module {..} = vsep $ map pretty _modImports ++ map pretty _modDeclarations

instance Pretty Import where
  pretty Import {..} = hsep
    [ "use"
    , hcat $ intersperse "::" (map pretty $ toList _importPath)
    , case _importType of
        Qualified (Just name) -> "as" <+> pretty name
        Qualified _           -> mempty
        Specific  names       -> encloseSep "::{" "}" "," (map pretty $ NE.toList names)
        Exhaustive            -> "::*"
    ] <> ";"

instance Pretty (Declaration p) where
  pretty = \case
    TypeAliasDecl _ tai -> pretty tai
    EnumDecl      _ ei  -> pretty ei
    StructDecl    _ si  -> pretty si
    ConstDecl     _ ci  -> pretty ci
    FunctionDecl  _ fi  -> pretty fi

instance Pretty (TypeAliasInfo p) where
  pretty TypeAliasInfo {..} = hsep
    [ "type"
    , pretty _aliasName
    , prettyParams _aliasParams
    , "="
    , pretty _aliasValue
    ] <> ";"

instance Pretty (EnumInfo p) where
  pretty EnumInfo {..} = hsep
    [ "enum"
    , pretty _enumName
    , encloseSep "{" "}" "," $ map pretty _enumValues
    ]

instance Pretty (StructInfo p) where
  pretty StructInfo {..} = hsep
    [ "struct"
    , pretty _structName
    , prettyParams _structParams
    , encloseSep "{" "}" "," do
        (name, typeExpr) <- NE.toList _structValues
        pure $ hsep
          [ pretty name
          , ":"
          , pretty typeExpr
          ]
    ]

instance Pretty (ConstInfo p) where
  pretty (ConstInfo {..}) = hsep
    [ "const"
    , pretty _constName
    , ":"
    , pretty _constType
    , "="
    , pretty _constExpr
    ] <> ";"

instance Pretty (FunctionInfo p) where
  pretty FunctionInfo {..} = hsep
    [ "fn"
    , pretty _funName
    , prettyParams _funParams
    , encloseSep "(" ")" "," do
        (name, argType) <- _funArgs
        pure $ hsep
          [ pretty name
          , ":"
          , case argType of
              ByValue     te -> pretty te
              ByReference te -> "&" <+> pretty te
          ]
    , foldMap (\t -> "->" <+> pretty t) _funType
    , prettyBlock _funBody
    ]

instance Pretty (Statement p) where
  pretty = \case
    IfStmt         _ ii -> pretty ii
    ForStmt        _ fi -> pretty fi
    WhileStmt      _ wi -> pretty wi
    LetStmt        _ li -> pretty li
    ReturnStmt     _ rs -> "return" <+> foldMap pretty rs <> ";"
    ContinueStmt   _    -> "continue;"
    BreakStmt      _    -> "break;"
    ExpressionStmt _  e -> pretty e <> ";"

instance Pretty (IfInfo p) where
  pretty IfInfo {..} = hsep
    [ "if"
    , pretty _ifExpr
    , prettyBlock _ifBody
    , case _ifElse of
        Nothing             -> mempty
        Just (ElseIf    ii) -> "else" <+> pretty ii
        Just (ElseBlock  b) -> "else" <+> prettyBlock b
    ]

instance Pretty (ForInfo p) where
  pretty ForInfo {..} = hsep
    [ "for"
    , pretty _forVariableName
    , "in"
    , pretty _forRangeExpr
    , prettyBlock _forBody
    ]

instance Pretty (WhileInfo p) where
  pretty WhileInfo {..} = hsep
    [ "while"
    , pretty _whileExpr
    , prettyBlock _whileBody
    ]

instance Pretty (LetInfo p) where
  pretty LetInfo {..} = hsep
    [ "let"
    , pretty _letName
    , foldMap (\t -> ":" <+> pretty t) _letType
    , "="
    , pretty _letExpr
    ] <> ";"

instance Pretty (Expression p) where
  pretty = \case
    PathExpr                     _ p     -> pretty p
    CastExpr                     _ e  t  -> parens (pretty e) <+> "as" <+> parens (pretty t)
    FieldAccessExpr              _ e  i  -> parens (pretty e) <> "." <> pretty i
    CallExpr                     _ f  as -> parens (pretty f) <> encloseSep "(" ")" "," (map pretty as)
    ArrayExpr                    _ vs    -> list $ map pretty vs
    IndexExpr                    _ e1 e2 -> parens (pretty e1) <> brackets (pretty e2)
    StructExpr                   _ p  fs -> parens (pretty p) <> encloseSep "{" "}" "," [pretty name <+> ":" <+> pretty value | (name, value) <- fs]
    BoolLiteralExpr              _ b     -> if b then "true" else "false"
    IntLiteralExpr               _ i     -> viaShow i
    CharLiteralExpr              _ c     -> viaShow c
    StringLiteralExpr            _ s     -> viaShow s
    ReferenceExpr                _ e     -> "&" <> parens (pretty e)
    NegationExpr                 _ e     -> "!" <> parens (pretty e)
    AdditionExpr                 _ e1 e2 -> parens (pretty e1) <+> "+"   <+> parens (pretty e2)
    SubtractionExpr              _ e1 e2 -> parens (pretty e1) <+> "-"   <+> parens (pretty e2)
    MultiplicationExpr           _ e1 e2 -> parens (pretty e1) <+> "*"   <+> parens (pretty e2)
    DivisionExpr                 _ e1 e2 -> parens (pretty e1) <+> "/"   <+> parens (pretty e2)
    ModuloExpr                   _ e1 e2 -> parens (pretty e1) <+> "%"   <+> parens (pretty e2)
    ExponentiationExpr           _ e1 e2 -> parens (pretty e1) <+> "^"   <+> parens (pretty e2)
    EqualityExpr                 _ e1 e2 -> parens (pretty e1) <+> "=="  <+> parens (pretty e2)
    DifferenceExpr               _ e1 e2 -> parens (pretty e1) <+> "!="  <+> parens (pretty e2)
    GreaterExpr                  _ e1 e2 -> parens (pretty e1) <+> "> "  <+> parens (pretty e2)
    LesserExpr                   _ e1 e2 -> parens (pretty e1) <+> "< "  <+> parens (pretty e2)
    GreaterEqExpr                _ e1 e2 -> parens (pretty e1) <+> ">="  <+> parens (pretty e2)
    LesserEqExpr                 _ e1 e2 -> parens (pretty e1) <+> "<="  <+> parens (pretty e2)
    BoolAndExpr                  _ e1 e2 -> parens (pretty e1) <+> "&&"  <+> parens (pretty e2)
    BoolOrExpr                   _ e1 e2 -> parens (pretty e1) <+> "||"  <+> parens (pretty e2)
    RangeInclusiveExpr           _ e1 e2 -> parens (pretty e1) <+> "..=" <+> parens (pretty e2)
    RangeExclusiveExpr           _ e1 e2 -> parens (pretty e1) <+> ".."  <+> parens (pretty e2)
    AssignmentExpr               _ e1 e2 -> parens (pretty e1) <+> "="   <+> parens (pretty e2)
    AdditionAssignmentExpr       _ e1 e2 -> parens (pretty e1) <+> "+="  <+> parens (pretty e2)
    SubtractionAssignmentExpr    _ e1 e2 -> parens (pretty e1) <+> "-="  <+> parens (pretty e2)
    MultiplicationAssignmentExpr _ e1 e2 -> parens (pretty e1) <+> "*="  <+> parens (pretty e2)
    DivisionAssignmentExpr       _ e1 e2 -> parens (pretty e1) <+> "/="  <+> parens (pretty e2)
    ModuloAssignmentExpr         _ e1 e2 -> parens (pretty e1) <+> "%="  <+> parens (pretty e2)
    ExponentiationAssignmentExpr _ e1 e2 -> parens (pretty e1) <+> "^="  <+> parens (pretty e2)


instance Pretty (PathInfo p) where
  pretty PathInfo {..} = hcat (intersperse "::" (toList $ fmap pretty _pathName)) <> case _pathParams of
    [] -> mempty
    _  -> encloseSep "::<" ">" "," $ map pretty _pathParams


prettyParams :: Pretty p => [p] -> Doc ann
prettyParams []     = mempty
prettyParams params = encloseSep "<" ">" "," $ map pretty params

prettyBlock :: Pretty p => [p] -> Doc ann
prettyBlock = braces . enclose hardline hardline . indent 2 . vsep . map pretty

prettyPrint :: Module Parsed -> Text
prettyPrint = renderStrict . layoutPretty defaultLayoutOptions . pretty
