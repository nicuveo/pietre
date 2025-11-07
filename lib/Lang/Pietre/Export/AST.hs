module Lang.Pietre.Export.Ast where

import "this" Prelude



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

prettyBlock :: Block Parsed -> Doc ann
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
