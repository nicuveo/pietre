module Lang.Pietre.Export.PrettyPrinting.AST.Parsed
  ( prettyPrintText
  , prettyPrintHTML
  ) where

import "this" Prelude


import Data.List.NonEmpty                           qualified as NE
import Lucid
import Prettyprinter
import Prettyprinter.Lucid
import Prettyprinter.Render.Text
import Prettyprinter.Render.Util.SimpleDocTree

import Lang.Pietre.Export.PrettyPrinting.AST.Common
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location


--------------------------------------------------------------------------------
-- API

prettyPrintText :: Module -> Text
prettyPrintText =
  renderStrict . layoutSmart options . prettyModule
  where
    options = LayoutOptions Unbounded

prettyPrintHTML :: Module -> Html ()
prettyPrintHTML =
  renderHtml . treeForm . fmap htmlAnnotation . layoutSmart options . prettyModule
  where
    options = LayoutOptions Unbounded


--------------------------------------------------------------------------------
-- Annotations

data Annotation
  = IntLiteralAnn
  | CharLiteralAnn
  | StringLiteralAnn
  | KeywordAnn
  | PathAnn

htmlAnnotation :: Annotation -> Html () -> Html ()
htmlAnnotation = \case
  IntLiteralAnn    -> span_ [class_ "syntax-parsed-int-literal"]
  CharLiteralAnn   -> span_ [class_ "syntax-parsed-string-literal"]
  StringLiteralAnn -> span_ [class_ "syntax-parsed-string-literal"]
  KeywordAnn       -> span_ [class_ "syntax-parsed-keyword"]
  PathAnn          -> span_ [class_ "syntax-parsed-path"]


--------------------------------------------------------------------------------
-- Implementations

prettyModule :: Module -> Doc Annotation
prettyModule Module {..} = sepByNewlines
  [ vsep $ map (prettyImport . _located) _modImports
  , sepByNewlines $ map (prettyDefinition . _located) _modDefinitions
  ]

prettyImport :: Import -> Doc Annotation
prettyImport Import {..} =
  annotate KeywordAnn "use"
  <+> sepByColons (map prettyIdentifier $ toList _importPath)
  <>  case _importType of
        Qualified (Just name) -> annotate KeywordAnn " as" <+> prettyIdentifier name
        Qualified _           -> mempty
        Specific  names       -> encloseSep "::{" "}" ", " (map prettyIdentifier $ NE.toList names)
        Exhaustive            -> "::*"
  <>  ";"

prettyDefinition :: Definition -> Doc Annotation
prettyDefinition = \case
  TypeAliasDef tai -> prettyTypeAlias tai
  EnumDef      ei  -> prettyEnum ei
  StructDef    si  -> prettyStruct si
  ConstDef     ci  -> prettyConst ci
  FunctionDef  fi  -> prettyFunction fi

prettyTypeAlias :: TypeAliasInfo -> Doc Annotation
prettyTypeAlias TypeAliasInfo {..} = hsep
  [ annotate KeywordAnn "type"
  , prettyIdentifier _aliasName
  , prettyParams _aliasParams
  , "="
  , prettyType _aliasValue
  ] <> ";"

prettyEnum :: EnumInfo -> Doc Annotation
prettyEnum EnumInfo {..} = hsep
  [ annotate KeywordAnn "enum"
  , prettyIdentifier _enumName
  , encloseSep "{" "}" ", " $ map prettyIdentifier _enumValues
  ]

prettyStruct :: StructInfo -> Doc Annotation
prettyStruct StructInfo {..} = vsep
  [ annotate KeywordAnn "struct"
    <+> prettyIdentifier _structName
    <>  prettyParams _structParams
    <+> "{"
  , indent 2 $ vsep do
      (name, typeExpr) <- NE.toList _structValues
      pure $
        prettyIdentifier name
        <>  ":"
        <+> prettyType typeExpr
        <>  ","
  , "}"
  ]

prettyConst :: ConstInfo -> Doc Annotation
prettyConst ConstInfo {..} =
  annotate KeywordAnn "const"
  <+> prettyIdentifier _constName
  <>  ":"
  <+> prettyType _constType
  <+> "="
  <+> prettyExpression _constExpr
  <>  ";"

prettyFunction :: FunctionInfo -> Doc Annotation
prettyFunction FunctionInfo {..} =
  annotate KeywordAnn "fn"
  <+> prettyIdentifier _funName
  <>  prettyParams _funParams
  <>  encloseSep "(" ")" ", " do
        (name, argType) <- _funArgs
        pure $ hcat
          [ prettyIdentifier name
          , ": "
          , case argType of
              ByValue     te -> prettyType te
              ByReference te -> "&" <> prettyType te
          ]
  <>  foldMap (\t -> " ->" <+> prettyType t) _funReturn
  <+> prettyBlock _funBody
  where
    FunctionType {..} = _funType

prettyStatement :: Statement -> Doc Annotation
prettyStatement = \case
  IfStmt         ii -> prettyIf ii
  ForStmt        fi -> prettyFor fi
  WhileStmt      wi -> prettyWhile wi
  LetStmt        li -> prettyLet li
  ReturnStmt     rs -> prettyReturn rs
  ExpressionStmt  e -> prettyExpression e <> ";"
  ContinueStmt      -> annotate KeywordAnn "continue" <> ";"
  BreakStmt         -> annotate KeywordAnn "break" <> ";"

prettyReturn :: Maybe (WithLocation Expression) -> Doc Annotation
prettyReturn = \case
  Just e  -> annotate KeywordAnn "return" <+> prettyExpression e <> ";"
  Nothing -> annotate KeywordAnn "return" <> ";"

prettyIf :: IfInfo -> Doc Annotation
prettyIf IfInfo {..} = hsep $
  [ annotate KeywordAnn "if"
  , prettyExpression _ifExpr
  , prettyBlock _ifBody
  ] <>
  case _ifElse of
    Nothing             -> []
    Just (ElseIf    ii) -> [annotate KeywordAnn "else", prettyIf ii]
    Just (ElseBlock  b) -> [annotate KeywordAnn "else", prettyBlock b]

prettyFor :: ForInfo -> Doc Annotation
prettyFor ForInfo {..} = hsep
  [ annotate KeywordAnn "for"
  , prettyIdentifier _forVariableName
  , annotate KeywordAnn "in"
  , prettyExpression _forRangeExpr
  , prettyBlock _forBody
  ]

prettyWhile :: WhileInfo -> Doc Annotation
prettyWhile WhileInfo {..} = hsep
  [ annotate KeywordAnn "while"
  , prettyExpression _whileExpr
  , prettyBlock _whileBody
  ]

prettyLet :: LetInfo -> Doc Annotation
prettyLet LetInfo {..} =
  annotate KeywordAnn "let"
  <+> prettyIdentifier _letName
  <>  foldMap (\t -> ":" <+> prettyType t) _letType
  <+> "="
  <+> prettyExpression _letExpr
  <>  ";"

prettyExpression :: WithLocation Expression -> Doc Annotation
prettyExpression = go
  where
    go = _located >>> \case
      PathExpr p -> prettyPath p
      CastExpr e t -> parens (go e) <+> annotate KeywordAnn "as" <+> prettyType t
      FieldAccessExpr e i -> parens (go e) <> "." <> prettyIdentifier i
      CallExpr f as -> prettyPath f <> encloseSep "(" ")" ", " (map go as)
      ArrayExpr vs -> list $ map go vs
      IndexExpr e1 e2 -> parens (go e1) <> brackets (go e2)
      StructExpr p fs -> prettyStructExpression p fs
      BoolLiteralExpr b -> annotate KeywordAnn $ if b then "true" else "false"
      IntLiteralExpr i -> annotate IntLiteralAnn $ viaShow i
      CharLiteralExpr c -> annotate CharLiteralAnn $ viaShow c
      StringLiteralExpr s -> annotate StringLiteralAnn $ viaShow s
      ReferenceExpr e -> "&" <> prettyPath e
      IntNegationExpr e -> "-" <> parens (go e)
      BoolNegationExpr e -> "!" <> parens (go e)
      AdditionExpr                 e1 e2 -> parens (go e1) <+> "+"   <+> parens (go e2)
      SubtractionExpr              e1 e2 -> parens (go e1) <+> "-"   <+> parens (go e2)
      MultiplicationExpr           e1 e2 -> parens (go e1) <+> "*"   <+> parens (go e2)
      DivisionExpr                 e1 e2 -> parens (go e1) <+> "/"   <+> parens (go e2)
      ModuloExpr                   e1 e2 -> parens (go e1) <+> "%"   <+> parens (go e2)
      ExponentiationExpr           e1 e2 -> parens (go e1) <+> "^"   <+> parens (go e2)
      EqualityExpr                 e1 e2 -> parens (go e1) <+> "=="  <+> parens (go e2)
      DifferenceExpr               e1 e2 -> parens (go e1) <+> "!="  <+> parens (go e2)
      GreaterExpr                  e1 e2 -> parens (go e1) <+> ">"   <+> parens (go e2)
      LesserExpr                   e1 e2 -> parens (go e1) <+> "<"   <+> parens (go e2)
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
    prettyStructExpression p fs =
      prettyPath p <>
        encloseSep "@{" "}" ", " do
          (name, value) <- NE.toList fs
          pure $ prettyIdentifier name <> ":" <+> go value

prettyParams :: [Identifier] -> Doc Annotation
prettyParams =
  prettyParamsWith prettyIdentifier "<"

prettyPath :: PathInfo -> Doc Annotation
prettyPath PathInfo {..} =
  annotate PathAnn $
  sepByColons (toList $ fmap prettyIdentifier _pathBase) <>
  prettyParamsWith prettyType "::<" _pathParams

prettyType :: PathInfo -> Doc Annotation
prettyType PathInfo {..} =
  annotate PathAnn $
  sepByColons (toList $ fmap prettyIdentifier _pathBase) <>
  prettyParamsWith prettyType "<" _pathParams

prettyParamsWith
  :: (p -> Doc Annotation)
  -> Doc Annotation
  -> [p]
  -> Doc Annotation
prettyParamsWith renderFun opening params
  | null params = mempty
  | otherwise   = encloseSep opening ">" ", " $ map renderFun params

prettyBlock :: Block -> Doc Annotation
prettyBlock block = vsep
  [ "{"
  , indent 2 $ vsep $ map (prettyStatement . _located) block
  , "}"
  ]

prettyIdentifier :: Identifier -> Doc Annotation
prettyIdentifier = pretty . rawIdentifier
