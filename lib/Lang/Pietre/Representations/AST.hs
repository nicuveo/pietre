{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Representations.AST where

import                "this" Prelude

import                Control.Lens
import                Data.Kind
import                Data.List.NonEmpty                   qualified as NE
import                Prettyprinter
import                Prettyprinter.Render.Text

import                Lang.Pietre.Representations.Location
import {-# SOURCE #-} Lang.Pietre.Representations.Name
import                Lang.Pietre.Representations.Tokens


--------------------------------------------------------------------------------
-- AST Phase

data ASTPhase = Parsed | Resolved

class Annotation (inner :: ASTPhase -> Type) (phase :: ASTPhase) where
  type Annotated inner phase
  within :: Lens' (Annotated inner phase) (inner phase)


instance Annotation Definition Parsed where
  type Annotated Definition Parsed = WithLocation (Definition Parsed)
  within = located

instance Annotation Statement Parsed where
  type Annotated Statement Parsed = WithLocation (Statement Parsed)
  within = located

instance Annotation Expression Parsed where
  type Annotated Expression Parsed = WithLocation (Expression Parsed)
  within = located


instance Annotation Definition Resolved where
  type Annotated Definition Resolved = WithLocation (Definition Resolved)
  within = located

instance Annotation Statement Resolved where
  type Annotated Statement Resolved = Statement Resolved
  within = id


class ASTRepresentation (p :: ASTPhase) where
  type NameType p :: Type

instance ASTRepresentation Parsed where
  type NameType Parsed = Path

instance ASTRepresentation Resolved where
  type NameType Resolved = Role


type ShowConstraints p =
  ( Show (Annotated Definition p)
  , Show (Annotated Statement   p)
  , Show (Annotated Expression  p)
  , Show (NameType p)
  )


--------------------------------------------------------------------------------
-- Parsed phase

data Module = Module
  { _modImports     :: [Import]
  , _modDefinitions :: [Annotated Definition Parsed]
  }

deriving instance Show Module

instance Semigroup Module where
  Module imports1 decls1 <> Module imports2 decls2 =
    Module (imports1 <> imports2) (decls1 <> decls2)

instance Monoid Module where
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


type Path = NonEmpty Identifier


data TypedExpression = TypedExpression
  { _exprIsLValue :: Bool
  , _exprType     :: PathInfo Resolved
  , _exprValue    :: Expression Resolved
  }

deriving instance ShowConstraints Resolved => Show TypedExpression

pattern LValueExpression
  :: PathInfo Resolved
  -> Expression Resolved
  -> TypedExpression
pattern LValueExpression eType eValue = TypedExpression True eType eValue

pattern RValueExpression
  :: PathInfo Resolved
  -> Expression Resolved
  -> TypedExpression
pattern RValueExpression eType eValue = TypedExpression False eType eValue


--------------------------------------------------------------------------------
-- Generic AST

data Definition (p :: ASTPhase)
  = TypeAliasDef (TypeAliasInfo p)
  | EnumDef      (EnumInfo      p)
  | StructDef    (StructInfo    p)
  | ConstDef     (ConstInfo     p)
  | FunctionDef  (FunctionInfo  p)

deriving instance ShowConstraints p => Show (Definition p)

data TypeAliasInfo (p :: ASTPhase) = TypeAliasInfo
  { _aliasName   :: Identifier
  , _aliasParams :: [Identifier]
  , _aliasValue  :: PathInfo p
  }

deriving instance ShowConstraints p => Show (TypeAliasInfo p)


data EnumInfo (p :: ASTPhase) = EnumInfo
  { _enumName   :: Identifier
  , _enumValues :: [Identifier]
  }

deriving instance ShowConstraints p => Show (EnumInfo p)


data StructInfo (p :: ASTPhase) = StructInfo
  { _structName   :: Identifier
  , _structParams :: [Identifier]
  , _structValues :: NonEmpty (Identifier, PathInfo p)
  }

deriving instance ShowConstraints p => Show (StructInfo p)


data ConstInfo (p :: ASTPhase) = ConstInfo
  { _constName :: Identifier
  , _constType :: PathInfo p
  , _constExpr :: Annotated Expression p
  }

deriving instance ShowConstraints p => Show (ConstInfo p)


data FunctionInfo (p :: ASTPhase) = FunctionInfo
  { _funName :: Identifier
  , _funType :: FunctionType p
  , _funBody :: [Annotated Statement p]
  }

deriving instance ShowConstraints p => Show (FunctionInfo p)

data FunctionType (p :: ASTPhase) = FunctionType
  { _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, FunctionArgType p)]
  , _funReturn :: Maybe (PathInfo p)
  } deriving Generic

deriving instance Eq  (FunctionType Resolved)
deriving instance Ord (FunctionType Resolved)
instance Hashable (FunctionType Resolved)

deriving instance ShowConstraints p => Show (FunctionType p)

data FunctionArgType (p :: ASTPhase)
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


data Statement (p :: ASTPhase)
  = IfStmt         (IfInfo    p)
  | ForStmt        (ForInfo   p)
  | WhileStmt      (WhileInfo p)
  | LetStmt        (LetInfo   p)
  | ReturnStmt     (Maybe (Annotated Expression p))
  | ContinueStmt
  | BreakStmt
  | ExpressionStmt (Annotated Expression p)

deriving instance ShowConstraints p => Show (Statement p)


data IfInfo (p :: ASTPhase) = IfInfo
  { _ifExpr :: Annotated Expression p
  , _ifBody :: [Annotated Statement p]
  , _ifElse :: Maybe (ElseInfo p)
  }

deriving instance ShowConstraints p => Show (IfInfo p)


data ElseInfo (p :: ASTPhase)
  = ElseIf    (IfInfo p)
  | ElseBlock [Annotated Statement p]

deriving instance ShowConstraints p => Show (ElseInfo p)


data ForInfo (p :: ASTPhase) = ForInfo
  { _forVariableName :: Identifier
  , _forRangeExpr    :: Annotated Expression p
  , _forBody         :: [Annotated Statement p]
  }

deriving instance ShowConstraints p => Show (ForInfo p)


data WhileInfo (p :: ASTPhase) = WhileInfo
  { _whileExpr :: Annotated Expression p
  , _whileBody :: [Annotated Statement p]
  }

deriving instance ShowConstraints p => Show (WhileInfo p)


data LetInfo (p :: ASTPhase) = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (PathInfo p)
  , _letExpr :: Annotated Expression p
  }

deriving instance ShowConstraints p => Show (LetInfo p)


data Expression (p :: ASTPhase)
  = PathExpr                     (PathInfo p)
  | FieldAccessExpr              (Annotated Expression p) Identifier
  | CallExpr                     (PathInfo p) [Annotated Expression p]
  | ArrayExpr                    [Annotated Expression p]
  | IndexExpr                    (Annotated Expression p) (Annotated Expression p)
  | StructExpr                   (PathInfo p) (NonEmpty (Identifier, Annotated Expression p))
  | BoolLiteralExpr              Bool
  | IntLiteralExpr               Int
  | CharLiteralExpr              Char
  | StringLiteralExpr            Text
  | ReferenceExpr                (PathInfo p)
  | IntNegationExpr              (Annotated Expression p)
  | BoolNegationExpr             (Annotated Expression p)
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
  | CastExpr                     (Annotated Expression p) (PathInfo p)
  | RangeInclusiveExpr           (Annotated Expression p) (Annotated Expression p)
  | RangeExclusiveExpr           (Annotated Expression p) (Annotated Expression p)
  | AssignmentExpr               (Annotated Expression p) (Annotated Expression p)
  | AdditionAssignmentExpr       (Annotated Expression p) (Annotated Expression p)
  | SubtractionAssignmentExpr    (Annotated Expression p) (Annotated Expression p)
  | MultiplicationAssignmentExpr (Annotated Expression p) (Annotated Expression p)
  | DivisionAssignmentExpr       (Annotated Expression p) (Annotated Expression p)
  | ModuloAssignmentExpr         (Annotated Expression p) (Annotated Expression p)
  | ExponentiationAssignmentExpr (Annotated Expression p) (Annotated Expression p)

deriving instance ShowConstraints p => Show (Expression p)


data PathInfo (p :: ASTPhase) = PathInfo
  { _pathName   :: NameType p
  , _pathParams :: [PathInfo p]
  } deriving (Generic)

deriving instance Eq  (PathInfo Resolved)
deriving instance Ord (PathInfo Resolved)
instance Hashable (PathInfo Resolved)

deriving instance ShowConstraints p => Show (PathInfo p)


--------------------------------------------------------------------------------
-- Pretty print

instance Pretty Module where
  pretty Module {..} =
    vsep $ map pretty _modImports ++ map (pretty . view located) _modDefinitions

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

instance Pretty (Definition Parsed) where
  pretty = \case
    TypeAliasDef tai -> pretty tai
    EnumDef      ei  -> pretty ei
    StructDef    si  -> pretty si
    ConstDef     ci  -> pretty ci
    FunctionDef  fi  -> pretty fi

instance Pretty (TypeAliasInfo Parsed) where
  pretty TypeAliasInfo {..} = hsep
    [ "type"
    , pretty _aliasName
    , prettyParams pretty "<" _aliasParams
    , "="
    , prettyTypeExpr _aliasValue
    ] <> ";"

instance Pretty (EnumInfo Parsed) where
  pretty EnumInfo {..} = hsep
    [ "enum"
    , pretty _enumName
    , encloseSep "{" "}" "," $ map pretty _enumValues
    ]

instance Pretty (StructInfo Parsed) where
  pretty StructInfo {..} = hsep
    [ "struct"
    , pretty _structName
    , prettyParams pretty "<" _structParams
    , encloseSep "{" "}" "," do
        (name, typeExpr) <- NE.toList _structValues
        pure $ hsep
          [ pretty name
          , ":"
          , prettyTypeExpr typeExpr
          ]
    ]

instance Pretty (ConstInfo Parsed) where
  pretty (ConstInfo {..}) = hsep
    [ "const"
    , pretty _constName
    , ":"
    , prettyTypeExpr _constType
    , "="
    , pretty (_constExpr ^. within @Expression @Parsed)
    ] <> ";"

instance Pretty (FunctionInfo Parsed) where
  pretty FunctionInfo {..} = hsep
    [ "fn"
    , pretty _funName
    , prettyParams pretty "<" _funParams
    , encloseSep "(" ")" "," do
        (name, argType) <- _funArgs
        pure $ hsep
          [ pretty name
          , ":"
          , case argType of
              ByValue     te -> prettyTypeExpr te
              ByReference te -> "&" <+> prettyTypeExpr te
          ]
    , foldMap (\t -> "->" <+> prettyTypeExpr t) _funReturn
    , prettyBlock _funBody
    ]
    where
      FunctionType {..} = _funType

instance Pretty (Statement Parsed) where
  pretty = \case
    IfStmt         ii -> pretty ii
    ForStmt        fi -> pretty fi
    WhileStmt      wi -> pretty wi
    LetStmt        li -> pretty li
    ReturnStmt     rs -> "return" <+> foldMap (pretty . (^. within @Expression @Parsed)) rs <> ";"
    ContinueStmt      -> "continue;"
    BreakStmt         -> "break;"
    ExpressionStmt  e -> pretty (e ^. within @Expression @Parsed) <> ";"

instance Pretty (IfInfo Parsed) where
  pretty IfInfo {..} = hsep
    [ "if"
    , pretty (_ifExpr ^. within @Expression @Parsed)
    , prettyBlock _ifBody
    , case _ifElse of
        Nothing             -> mempty
        Just (ElseIf    ii) -> "else" <+> pretty ii
        Just (ElseBlock  b) -> "else" <+> prettyBlock b
    ]

instance Pretty (ForInfo Parsed) where
  pretty ForInfo {..} = hsep
    [ "for"
    , pretty _forVariableName
    , "in"
    , pretty (_forRangeExpr ^. within @Expression @Parsed)
    , prettyBlock _forBody
    ]

instance Pretty (WhileInfo Parsed) where
  pretty WhileInfo {..} = hsep
    [ "while"
    , pretty (_whileExpr ^. within @Expression @Parsed)
    , prettyBlock _whileBody
    ]

instance Pretty (LetInfo Parsed) where
  pretty LetInfo {..} = hsep
    [ "let"
    , pretty _letName
    , foldMap (\t -> ":" <+> prettyTypeExpr t) _letType
    , "="
    , pretty (_letExpr ^. within @Expression @Parsed)
    ] <> ";"

instance Pretty (Expression Parsed) where
  pretty = \case
    PathExpr                     p     -> prettyPathExpr p
    CastExpr                     e  t  -> parens (go e) <+> "as" <+> prettyPathExpr t
    FieldAccessExpr              e  i  -> parens (go e) <> "." <> pretty i
    CallExpr                     f  as -> prettyPathExpr f <> encloseSep "(" ")" "," (map go as)
    ArrayExpr                    vs    -> list $ map go vs
    IndexExpr                    e1 e2 -> parens (go e1) <> brackets (go e2)
    StructExpr                   p  fs -> prettyPathExpr p <+> encloseSep "@{" "}" "," [pretty name <+> ":" <+> go value | (name, value) <- NE.toList fs]
    BoolLiteralExpr              b     -> if b then "true" else "false"
    IntLiteralExpr               i     -> viaShow i
    CharLiteralExpr              c     -> viaShow c
    StringLiteralExpr            s     -> viaShow s
    ReferenceExpr                e     -> "&" <> prettyPathExpr e
    IntNegationExpr              e     -> "-" <> parens (go e)
    BoolNegationExpr             e     -> "!" <> parens (go e)
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
      go = pretty . (^. within @Expression @Parsed)

prettyPathExpr :: PathInfo Parsed -> Doc ann
prettyPathExpr PathInfo {..} = hcat (intersperse "::" (toList $ fmap pretty _pathName)) <> prettyParams prettyTypeExpr "::<" _pathParams

prettyTypeExpr :: PathInfo Parsed -> Doc ann
prettyTypeExpr PathInfo {..} = hcat (intersperse "::" (toList $ fmap pretty _pathName)) <> prettyParams prettyTypeExpr "<" _pathParams

prettyParams :: (p -> Doc ann) -> Doc ann -> [p] -> Doc ann
prettyParams renderFun opening params
  | null params = mempty
  | otherwise   = encloseSep opening ">" "," $ map renderFun params

prettyBlock :: [Annotated Statement Parsed] -> Doc ann
prettyBlock = braces . enclose hardline hardline . indent 2 . vsep . map (pretty . (^. within @Statement @Parsed))

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


--------------------------------------------------------------------------------
-- Order-dependent declarations

-- Due to lenses, some declarations must be put at the end of the file, *after*
-- the corresponding lens declaration.

instance Annotation Expression Resolved where
  type Annotated Expression Resolved = TypedExpression
  within = exprValue
