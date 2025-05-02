{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Representations.AST where

import "this" Prelude

import Control.Lens
import Data.Kind
import Data.List.NonEmpty                   qualified as NE
import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


--------------------------------------------------------------------------------
-- AST Phase

data ASTPhase = Parsed | Resolved

class Annotation (inner :: ASTPhase -> Type) (phase :: ASTPhase) where
  type Annotated inner phase
  within :: Lens' (Annotated inner phase) (inner phase)


instance Annotation Declaration Parsed where
  type Annotated Declaration Parsed = WithLocation (Declaration Parsed)
  within = located

instance Annotation Statement Parsed where
  type Annotated Statement Parsed = WithLocation (Statement Parsed)
  within = located

instance Annotation Expression Parsed where
  type Annotated Expression Parsed = WithLocation (Expression Parsed)
  within = located


instance Annotation Declaration Resolved where
  type Annotated Declaration Resolved = Declaration Resolved
  within = id

instance Annotation Statement Resolved where
  type Annotated Statement Resolved = Statement Resolved
  within = id

instance Annotation Expression Resolved where
  type Annotated Expression Resolved = Expression Resolved
  within = id


class ASTRepresentation (p :: ASTPhase) where
  type NameType p :: Type

instance ASTRepresentation Parsed where
  type NameType Parsed = PathInfo

instance ASTRepresentation Resolved where
  type NameType Resolved = Name


type ASTConstraints p =
  ( Show (Annotated Declaration p)
  , Show (Annotated Statement   p)
  , Show (Annotated Expression  p)
  , Show (NameType p)
  , Pretty (NameType p)
  , Annotation Declaration p
  , Annotation Statement   p
  , Annotation Expression  p
  )


--------------------------------------------------------------------------------
-- Parsed phase

data Module = Module
  { _modImports      :: [Import]
  , _modDeclarations :: [Annotated Declaration Parsed]
  }

deriving instance Show Module

instance Semigroup Module where
  Module imports1 decls1 <> Module imports2 decls2 =
    Module (imports1 <> imports2) (decls1 <> decls2)

instance Monoid Module where
  mempty = Module [] []


data PathInfo = PathInfo
  { _pathName   :: NonEmpty Identifier
  , _pathParams :: [PathInfo]
  } deriving Show


--------------------------------------------------------------------------------
-- Generic AST

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
  = TypeAliasDecl (TypeAliasInfo p)
  | EnumDecl      (EnumInfo      p)
  | StructDecl    (StructInfo    p)
  | ConstDecl     (ConstInfo     p)
  | FunctionDecl  (FunctionInfo  p)

deriving instance ASTConstraints p => Show (Declaration p)


data TypeAliasInfo (p :: ASTPhase) = TypeAliasInfo
  { _aliasName   :: Identifier
  , _aliasParams :: [Identifier]
  , _aliasValue  :: NameType p
  }

deriving instance ASTConstraints p => Show (TypeAliasInfo p)


data EnumInfo (p :: ASTPhase) = EnumInfo
  { _enumName   :: Identifier
  , _enumValues :: [Identifier]
  }

deriving instance ASTConstraints p => Show (EnumInfo p)


data StructInfo (p :: ASTPhase) = StructInfo
  { _structName   :: Identifier
  , _structParams :: [Identifier]
  , _structValues :: NonEmpty (Identifier, NameType p)
  }

deriving instance ASTConstraints p => Show (StructInfo p)


data ConstInfo (p :: ASTPhase) = ConstInfo
  { _constName :: Identifier
  , _constType :: NameType p
  , _constExpr :: Annotated Expression p
  }

deriving instance ASTConstraints p => Show (ConstInfo p)


data FunctionInfo (p :: ASTPhase) = FunctionInfo
  { _funName   :: Identifier
  , _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, FunctionArgType p)]
  , _funType   :: Maybe (NameType p)
  , _funBody   :: [Annotated Statement p]
  }

deriving instance ASTConstraints p => Show (FunctionInfo p)


data FunctionArgType (p :: ASTPhase)
  = ByValue     (NameType p)
  | ByReference (NameType p)

deriving instance ASTConstraints p => Show (FunctionArgType p)


data Statement (p :: ASTPhase)
  = IfStmt         (IfInfo    p)
  | ForStmt        (ForInfo   p)
  | WhileStmt      (WhileInfo p)
  | LetStmt        (LetInfo   p)
  | ReturnStmt     (Maybe (Annotated Expression p))
  | ContinueStmt
  | BreakStmt
  | ExpressionStmt (Annotated Expression p)

deriving instance ASTConstraints p => Show (Statement p)


data IfInfo (p :: ASTPhase) = IfInfo
  { _ifExpr :: Annotated Expression p
  , _ifBody :: [Annotated Statement p]
  , _ifElse :: Maybe (ElseInfo p)
  }

deriving instance ASTConstraints p => Show (IfInfo p)


data ElseInfo (p :: ASTPhase)
  = ElseIf    (IfInfo p)
  | ElseBlock [Annotated Statement p]

deriving instance ASTConstraints p => Show (ElseInfo p)


data ForInfo (p :: ASTPhase) = ForInfo
  { _forVariableName :: Identifier
  , _forRangeExpr    :: Annotated Expression p
  , _forBody         :: [Annotated Statement p]
  }

deriving instance ASTConstraints p => Show (ForInfo p)


data WhileInfo (p :: ASTPhase) = WhileInfo
  { _whileExpr :: Annotated Expression p
  , _whileBody :: [Annotated Statement p]
  }

deriving instance ASTConstraints p => Show (WhileInfo p)


data LetInfo (p :: ASTPhase) = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (NameType p)
  , _letExpr :: Annotated Expression p
  }

deriving instance ASTConstraints p => Show (LetInfo p)


data Expression (p :: ASTPhase)
  = PathExpr                     (NameType p)
  | FieldAccessExpr              (Annotated Expression p) Identifier
  | CallExpr                     (NameType p) [Annotated Expression p]
  | ArrayExpr                    [Annotated Expression p]
  | IndexExpr                    (Annotated Expression p) (Annotated Expression p)
  | StructExpr                   (NameType p) (NonEmpty (Identifier, Annotated Expression p))
  | BoolLiteralExpr              Bool
  | IntLiteralExpr               Int
  | CharLiteralExpr              Char
  | StringLiteralExpr            Text
  | ReferenceExpr                (NameType p)
  | NegationExpr                 (Annotated Expression p)
  | AdditionExpr                 (Annotated Expression p) (Annotated Expression p)
  | SubtractionExpr              (Annotated Expression p) (Annotated Expression p)
  | MultiplicationExpr           (Annotated Expression p) (Annotated Expression p)
  | DivisionExpr                 (Annotated Expression p) (Annotated Expression p)
  | ModuloExpr                   (Annotated Expression p) (Annotated Expression p)
  | ExponentiationExpr           (Annotated Expression p) (Annotated Expression p)
  | EqualityExpr                 (Annotated Expression p) (Annotated Expression p)
  | DifferenceExpr               (Annotated Expression p) (Annotated Expression p)
  | GreaterExpr                  (Annotated Expression p) (Annotated Expression p)
  | LesserExpr                   (Annotated Expression p) (Annotated Expression p)
  | GreaterEqExpr                (Annotated Expression p) (Annotated Expression p)
  | LesserEqExpr                 (Annotated Expression p) (Annotated Expression p)
  | BoolAndExpr                  (Annotated Expression p) (Annotated Expression p)
  | BoolOrExpr                   (Annotated Expression p) (Annotated Expression p)
  | CastExpr                     (Annotated Expression p) (NameType p)
  | RangeInclusiveExpr           (Annotated Expression p) (Annotated Expression p)
  | RangeExclusiveExpr           (Annotated Expression p) (Annotated Expression p)
  | AssignmentExpr               (Annotated Expression p) (Annotated Expression p)
  | AdditionAssignmentExpr       (Annotated Expression p) (Annotated Expression p)
  | SubtractionAssignmentExpr    (Annotated Expression p) (Annotated Expression p)
  | MultiplicationAssignmentExpr (Annotated Expression p) (Annotated Expression p)
  | DivisionAssignmentExpr       (Annotated Expression p) (Annotated Expression p)
  | ModuloAssignmentExpr         (Annotated Expression p) (Annotated Expression p)
  | ExponentiationAssignmentExpr (Annotated Expression p) (Annotated Expression p)

deriving instance ASTConstraints p => Show (Expression p)


--------------------------------------------------------------------------------
-- Pretty print

instance Pretty Module where
  pretty Module {..} =
    vsep $ map pretty _modImports ++ map (pretty . view located) _modDeclarations

instance Pretty PathInfo where
  pretty PathInfo {..} = hcat (intersperse "::" (toList $ fmap pretty _pathName)) <> case _pathParams of
    [] -> mempty
    _  -> encloseSep "::<" ">" "," $ map pretty _pathParams

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

instance (ASTConstraints p) => Pretty (Declaration p) where
  pretty = \case
    TypeAliasDecl tai -> pretty tai
    EnumDecl      ei  -> pretty ei
    StructDecl    si  -> pretty si
    ConstDecl     ci  -> pretty ci
    FunctionDecl  fi  -> pretty fi

instance (ASTConstraints p) => Pretty (TypeAliasInfo p) where
  pretty TypeAliasInfo {..} = hsep
    [ "type"
    , pretty _aliasName
    , prettyParams _aliasParams
    , "="
    , pretty _aliasValue
    ] <> ";"

instance (ASTConstraints p) => Pretty (EnumInfo p) where
  pretty EnumInfo {..} = hsep
    [ "enum"
    , pretty _enumName
    , encloseSep "{" "}" "," $ map pretty _enumValues
    ]

instance (ASTConstraints p) => Pretty (StructInfo p) where
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

instance (ASTConstraints p) => Pretty (ConstInfo p) where
  pretty (ConstInfo {..}) = hsep
    [ "const"
    , pretty _constName
    , ":"
    , pretty _constType
    , "="
    , pretty (_constExpr ^. within @Expression @p)
    ] <> ";"

instance (ASTConstraints p) => Pretty (FunctionInfo p) where
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
    , prettyBlock @p _funBody
    ]

instance (ASTConstraints p) => Pretty (Statement p) where
  pretty = \case
    IfStmt         ii -> pretty ii
    ForStmt        fi -> pretty fi
    WhileStmt      wi -> pretty wi
    LetStmt        li -> pretty li
    ReturnStmt     rs -> "return" <+> foldMap (pretty . (^. within @Expression @p)) rs <> ";"
    ContinueStmt      -> "continue;"
    BreakStmt         -> "break;"
    ExpressionStmt  e -> pretty (e ^. within @Expression @p) <> ";"

instance (ASTConstraints p) => Pretty (IfInfo p) where
  pretty IfInfo {..} = hsep
    [ "if"
    , pretty (_ifExpr ^. within @Expression @p)
    , prettyBlock @p _ifBody
    , case _ifElse of
        Nothing             -> mempty
        Just (ElseIf    ii) -> "else" <+> pretty ii
        Just (ElseBlock  b) -> "else" <+> prettyBlock @p b
    ]

instance (ASTConstraints p) => Pretty (ForInfo p) where
  pretty ForInfo {..} = hsep
    [ "for"
    , pretty _forVariableName
    , "in"
    , pretty (_forRangeExpr ^. within @Expression @p)
    , prettyBlock @p _forBody
    ]

instance (ASTConstraints p) => Pretty (WhileInfo p) where
  pretty WhileInfo {..} = hsep
    [ "while"
    , pretty (_whileExpr ^. within @Expression @p)
    , prettyBlock @p _whileBody
    ]

instance (ASTConstraints p) => Pretty (LetInfo p) where
  pretty LetInfo {..} = hsep
    [ "let"
    , pretty _letName
    , foldMap (\t -> ":" <+> pretty t) _letType
    , "="
    , pretty (_letExpr ^. within @Expression @p)
    ] <> ";"

instance (ASTConstraints p) => Pretty (Expression p) where
  pretty = \case
    PathExpr                     p     -> pretty p
    CastExpr                     e  t  -> parens (go e) <+> "as" <+> pretty t
    FieldAccessExpr              e  i  -> parens (go e) <> "." <> pretty i
    CallExpr                     f  as -> pretty f <> encloseSep "(" ")" "," (map go as)
    ArrayExpr                    vs    -> list $ map go vs
    IndexExpr                    e1 e2 -> parens (go e1) <> brackets (go e2)
    StructExpr                   p  fs -> pretty p <+> encloseSep "@{" "}" "," [pretty name <+> ":" <+> go value | (name, value) <- NE.toList fs]
    BoolLiteralExpr              b     -> if b then "true" else "false"
    IntLiteralExpr               i     -> viaShow i
    CharLiteralExpr              c     -> viaShow c
    StringLiteralExpr            s     -> viaShow s
    ReferenceExpr                e     -> "&" <> pretty e
    NegationExpr                 e     -> "!" <> parens (go e)
    AdditionExpr                 e1 e2 -> parens (go e1) <+> "+"   <+> parens (go e2)
    SubtractionExpr              e1 e2 -> parens (go e1) <+> "-"   <+> parens (go e2)
    MultiplicationExpr           e1 e2 -> parens (go e1) <+> "*"   <+> parens (go e2)
    DivisionExpr                 e1 e2 -> parens (go e1) <+> "/"   <+> parens (go e2)
    ModuloExpr                   e1 e2 -> parens (go e1) <+> "%"   <+> parens (go e2)
    ExponentiationExpr           e1 e2 -> parens (go e1) <+> "^"   <+> parens (go e2)
    EqualityExpr                 e1 e2 -> parens (go e1) <+> "=="  <+> parens (go e2)
    DifferenceExpr               e1 e2 -> parens (go e1) <+> "!="  <+> parens (go e2)
    GreaterExpr                  e1 e2 -> parens (go e1) <+> "> "  <+> parens (go e2)
    LesserExpr                   e1 e2 -> parens (go e1) <+> "< "  <+> parens (go e2)
    GreaterEqExpr                e1 e2 -> parens (go e1) <+> ">="  <+> parens (go e2)
    LesserEqExpr                 e1 e2 -> parens (go e1) <+> "<="  <+> parens (go e2)
    BoolAndExpr                  e1 e2 -> parens (go e1) <+> "&&"  <+> parens (go e2)
    BoolOrExpr                   e1 e2 -> parens (go e1) <+> "||"  <+> parens (go e2)
    RangeInclusiveExpr           e1 e2 -> parens (go e1) <+> "..=" <+> parens (go e2)
    RangeExclusiveExpr           e1 e2 -> parens (go e1) <+> ".."  <+> parens (go e2)
    AssignmentExpr               e1 e2 -> parens (go e1) <+> "="   <+> parens (go e2)
    AdditionAssignmentExpr       e1 e2 -> parens (go e1) <+> "+="  <+> parens (go e2)
    SubtractionAssignmentExpr    e1 e2 -> parens (go e1) <+> "-="  <+> parens (go e2)
    MultiplicationAssignmentExpr e1 e2 -> parens (go e1) <+> "*="  <+> parens (go e2)
    DivisionAssignmentExpr       e1 e2 -> parens (go e1) <+> "/="  <+> parens (go e2)
    ModuloAssignmentExpr         e1 e2 -> parens (go e1) <+> "%="  <+> parens (go e2)
    ExponentiationAssignmentExpr e1 e2 -> parens (go e1) <+> "^="  <+> parens (go e2)
    where
      go = pretty . (^. within @Expression @p)


prettyParams :: Pretty p => [p] -> Doc ann
prettyParams []     = mempty
prettyParams params = encloseSep "<" ">" "," $ map pretty params

prettyBlock :: forall p ann. ASTConstraints p => [Annotated Statement p] -> Doc ann
prettyBlock = braces . enclose hardline hardline . indent 2 . vsep . map (pretty . (^. within @Statement @p))

prettyPrint :: Pretty p => p -> Text
prettyPrint = renderStrict . layoutPretty defaultLayoutOptions . pretty


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

makePrisms ''ImportType
makePrisms ''Declaration
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
    NegationExpr                 e     -> fmap   NegationExpr                 (within f e)
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
