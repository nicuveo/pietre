module Lang.Pietre.Export.PrettyPrinting.AST.Validated
  ( prettyPrintText
  , prettyPrintHTML
  ) where

import "this" Prelude

import Data.HashMap.Strict.Extra                             qualified as M
import Data.List.NonEmpty                                    qualified as NE
import Data.Text                                             qualified as T
import Lucid                                                 hiding (for_)
import Lucid.Base                                            (makeAttribute)
import Prettyprinter
import Prettyprinter.Lucid
import Prettyprinter.Render.Text
import Prettyprinter.Render.Util.SimpleDocTree

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Export.PrettyPrinting.AST.Common
import Lang.Pietre.Export.PrettyPrinting.AST.Validated.Monad
import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


--------------------------------------------------------------------------------
-- API

prettyPrintText :: Interface -> Text
prettyPrintText = prettyInterface
  >>> runPrinter
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> renderStrict

prettyPrintHTML :: Interface -> Html ()
prettyPrintHTML = prettyInterface
  >>> runPrinter
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> fmap htmlAnnotation
  >>> treeForm
  >>> renderHtml


--------------------------------------------------------------------------------
-- Mangling

class Manglable a where
  mangle :: a -> Text

instance Manglable Identifier where
  mangle = rawIdentifier

instance Manglable BaseName where
  mangle BaseName {..} =
    T.intercalate "_" $ map mangle $ toList _nameModule <> [_nameIdent]

instance Manglable Name where
  mangle IntName   = "int"
  mangle CharName  = "bool"
  mangle BoolName  = "char"
  mangle UnitName  = "unit"
  mangle VoidName  = "void"
  mangle Name {..} = T.intercalate "_" (mangle _nameBase : map mangle _nameParams)


--------------------------------------------------------------------------------
-- Annotations

data Annotation
  = IntLiteralAnn
  | CharLiteralAnn
  | StringLiteralAnn
  | KeywordAnn
  | DeclarationAnn Text
  | VariableAnn Text
  | ReferenceAnn Text
  | TypeAnn Text
  | FunctionAnn Text
  | ParameterAnn Text

htmlAnnotation :: Annotation -> Html () -> Html ()
htmlAnnotation = \case
  IntLiteralAnn      -> span_ [class_ "syntax-validated-int-literal"]
  CharLiteralAnn     -> span_ [class_ "syntax-validated-string-literal"]
  StringLiteralAnn   -> span_ [class_ "syntax-validated-string-literal"]
  KeywordAnn         -> span_ [class_ "syntax-validated-keyword"]
  DeclarationAnn uid -> span_ [makeAttribute "declID" uid, id_ uid]
  VariableAnn uid    -> mkReference uid "syntax-validated-variable"
  ReferenceAnn uid   -> mkReference uid "syntax-validated-reference"
  TypeAnn uid        -> mkReference uid "syntax-validated-type"
  FunctionAnn uid    -> mkReference uid "syntax-validated-function"
  ParameterAnn uid   -> mkReference uid "syntax-validated-parameter"
  where
    mkReference uid className =
      span_ [makeAttribute "declid" uid, class_ className] . a_ [href_ $ "#" <> uid]


--------------------------------------------------------------------------------
-- Pretty printing

type Prettifier a = a -> Printer (Doc Annotation)

prettyIdentifier :: Identifier -> Doc Annotation
prettyIdentifier = pretty . rawIdentifier

prettyBaseName :: BaseName -> Doc Annotation
prettyBaseName BaseName {..} =
  sepByColons $ map prettyIdentifier $ NE.toList _nameModule ++ [_nameIdent]

prettyTypeName :: Prettifier Name
prettyTypeName IntName   = pure $ annotate KeywordAnn "int"
prettyTypeName CharName  = pure $ annotate KeywordAnn "bool"
prettyTypeName BoolName  = pure $ annotate KeywordAnn "char"
prettyTypeName UnitName  = pure $ annotate KeywordAnn "()"
prettyTypeName VoidName  = pure $ annotate KeywordAnn "!void"
prettyTypeName Name {..} = do
  typeID <- retrieve $ mangle _nameBase
  let tBaseName = annotate (TypeAnn typeID) $ prettyBaseName _nameBase
  tTypeArgs <- traverse prettyTypeName _nameParams
  pure $
    if null _nameParams
    then tBaseName
    else hcat [tBaseName, "<", sepByCommas tTypeArgs, ">"]

prettyConcreteType :: Prettifier ConcreteType
prettyConcreteType =
  prettyTypeWith prettyConcreteType

prettyParameterizedType :: Prettifier ParameterizedType
prettyParameterizedType = \case
  Left (_, paramName) -> do
    paramID <- retrieve $ mangle paramName
    pure $ annotate (ParameterAnn paramID) $ prettyIdentifier paramName
  Right actualType ->
    prettyTypeWith prettyParameterizedType actualType

prettyTypeWith
  :: (TypeTree f -> Printer (Doc Annotation))
  -> TypeNode f
  -> Printer (Doc Annotation)
prettyTypeWith go = \case
  IntType  -> pure $ annotate KeywordAnn "int"
  BoolType -> pure $ annotate KeywordAnn "bool"
  CharType -> pure $ annotate KeywordAnn "char"
  UnitType -> pure $ annotate KeywordAnn "()"
  VoidType -> pure $ annotate KeywordAnn "!void"
  EnumType baseName _ -> do
    enumID <- retrieve $ mangle baseName
    pure $ annotate (TypeAnn enumID) (prettyBaseName baseName)
  StructType StructTypeInfo {..} -> do
    structID <- retrieve $ mangle _structBaseName
    let structDoc = annotate (TypeAnn  structID) (prettyBaseName _structBaseName)
    paramDocs <- traverse go _structTypeParams
    pure $
      if null _structTypeParams
      then structDoc
      else hcat [structDoc, "<", sepByCommas paramDocs, ">"]
  FunctionType FunctionTypeInfo {..} -> do
    prettifiedArgs <- for _funArgs \(_, argType) -> case argType of
      ByValue     innerType -> go innerType
      ByReference innerType -> go innerType <&> ("&" <>)
    prettifiedReturn <- go _funReturn
    pure $ "(" <> sepByCommas prettifiedArgs <> ")" <+> "->" <+> prettifiedReturn

prettyFunctionName :: Prettifier Name
prettyFunctionName name@Name {..} = do
  funID <- retrieve $ mangle name
  let fBaseName = annotate (FunctionAnn funID) $ prettyBaseName _nameBase
  fTypeArgs <- traverse prettyTypeName _nameParams
  pure $
    if null _nameParams
    then fBaseName
    else hcat [fBaseName, "::<", sepByCommas fTypeArgs, ">"]

prettyFieldWith
  :: Prettifier e
  -> Prettifier (Identifier, e)
prettyFieldWith go (fieldName, fieldValue) = do
  let prettifiedName = prettyIdentifier fieldName
  prettifiedValue <- go fieldValue
  pure $ prettifiedName <> ":" <+> prettifiedValue

prettyExpression :: Prettifier (Typed Expression)
prettyExpression = go
  where
    go Typed {..} = case _typedValue of
      LocalVariableExpr i -> do
        varID <- retrieve $ mangle i
        pure $ annotate (VariableAnn varID) $ prettyIdentifier i
      ReferenceArgumentExpr i -> do
        varID <- retrieve $ mangle i
        pure $ annotate (ReferenceAnn varID) $ prettyIdentifier i
      IndexExpr e1 e2 -> do
        lhs <- go e1
        rhs <- go e2
        pure $ parens lhs <> brackets rhs
      FunctionNameExpr n _ ->
        prettyFunctionName n
      FunctionCallExpr n _ args -> do
        fName <- prettyFunctionName n
        fArgs <- traverse go args
        pure $ fName <> "(" <> sepByCommas fArgs <> ")"
      VariableCallExpr i _ args -> do
        varID <- retrieve $ mangle i
        let fName = annotate (VariableAnn varID) $ prettyIdentifier i
        fArgs <- traverse go args
        pure $ fName <> "(" <> sepByCommas fArgs <> ")"
      ArrayExpr values -> do
        prettifiedValues <- traverse go values
        pure $ "[" <> sepByCommas prettifiedValues <> "]"
      StructExpr _ fields -> do
        structType <- prettyConcreteType _typeInfo
        prettifiedFields <- traverse (prettyFieldWith go) $ NE.toList fields
        pure $ hsep
          [ structType
          , "{"
          , sepByCommas prettifiedFields
          , "}"
          ]
      FieldAccessExpr _ e i -> do
        prettifiedExpr <- go e
        pure $ parens prettifiedExpr <> "." <> prettyIdentifier i
      BoolLiteralExpr b ->
        pure $ annotate KeywordAnn $ if b then "true" else "false"
      IntLiteralExpr i ->
        pure $ annotate IntLiteralAnn $ viaShow i
      CharLiteralExpr c ->
        pure $ annotate CharLiteralAnn $ viaShow c
      StringLiteralExpr s ->
        pure $ annotate StringLiteralAnn $ viaShow s
      IntNegationExpr e -> do
        pe <- go e
        pure $ "-" <> parens pe
      BoolNegationExpr e -> do
        pe <- go e
        pure $ "!" <> parens pe
      CastExpr e t -> do
        pe <- go e
        pt <- prettyConcreteType t
        pure $ parens pe <+> annotate KeywordAnn "as" <+> pt
      RangeExpr Typed {..} ->
        prettyRangeExpression _typedValue
      AdditionExpr                 e1 e2 -> binaryOp go "+"  e1 e2
      SubtractionExpr              e1 e2 -> binaryOp go "-"  e1 e2
      MultiplicationExpr           e1 e2 -> binaryOp go "*"  e1 e2
      DivisionExpr                 e1 e2 -> binaryOp go "/"  e1 e2
      ModuloExpr                   e1 e2 -> binaryOp go "%"  e1 e2
      ExponentiationExpr           e1 e2 -> binaryOp go "^"  e1 e2
      EqualityExpr                 e1 e2 -> binaryOp go "==" e1 e2
      DifferenceExpr               e1 e2 -> binaryOp go "!=" e1 e2
      GreaterExpr                  e1 e2 -> binaryOp go ">"  e1 e2
      LesserExpr                   e1 e2 -> binaryOp go "<"  e1 e2
      GreaterEqExpr                e1 e2 -> binaryOp go ">=" e1 e2
      LesserEqExpr                 e1 e2 -> binaryOp go "<=" e1 e2
      BoolAndExpr                  e1 e2 -> binaryOp go "&&" e1 e2
      BoolOrExpr                   e1 e2 -> binaryOp go "||" e1 e2
      AssignmentExpr               e1 e2 -> binaryOp prettyLValueExpression "="  e1 e2
      AdditionAssignmentExpr       e1 e2 -> binaryOp prettyLValueExpression "+=" e1 e2
      SubtractionAssignmentExpr    e1 e2 -> binaryOp prettyLValueExpression "-=" e1 e2
      MultiplicationAssignmentExpr e1 e2 -> binaryOp prettyLValueExpression "*=" e1 e2
      DivisionAssignmentExpr       e1 e2 -> binaryOp prettyLValueExpression "/=" e1 e2
      ModuloAssignmentExpr         e1 e2 -> binaryOp prettyLValueExpression "%=" e1 e2
      ExponentiationAssignmentExpr e1 e2 -> binaryOp prettyLValueExpression "^=" e1 e2

    binaryOp
      :: Prettifier a
      -> Doc Annotation
      -> a
      -> Typed Expression
      -> Printer (Doc Annotation)
    binaryOp f op e1 e2 = do
      lhs <- f e1
      rhs <- go e2
      pure $ parens lhs <+> op <+> parens rhs

prettyLValueExpression :: Prettifier (Typed LValueExpression)
prettyLValueExpression = go
  where
    go = _typedValue >>> \case
      LocalVariableLExpr i -> do
        varID <- retrieve $ mangle i
        pure $ annotate (VariableAnn varID) $ prettyIdentifier i
      ReferenceArgumentLExpr i -> do
        varID <- retrieve $ mangle i
        pure $ annotate (ReferenceAnn varID) $ prettyIdentifier i
      IndexLExpr e1 e2 -> do
        lhs <- go e1
        rhs <- go e2
        pure $ parens lhs <> brackets rhs
      FieldAccessLExpr _ e i -> do
        prettifiedExpr <- go e
        pure $ parens prettifiedExpr <> "." <> prettyIdentifier i

prettyConstExpression :: Prettifier (Typed ConstExpression)
prettyConstExpression = go
  where
    go Typed {..} = case _typedValue of
      ArrayConstExpr values -> do
        prettifiedValues <- traverse go values
        pure $ "[" <> sepByCommas prettifiedValues <> "]"
      StructConstExpr _ fields -> do
        structType <- prettyConcreteType _typeInfo
        prettifiedFields <- traverse (prettyFieldWith go) $ NE.toList fields
        pure $ hsep
          [ structType
          , "{"
          , sepByCommas prettifiedFields
          , "}"
          ]
      BoolLiteralConstExpr b ->
        pure $ annotate KeywordAnn $ if b then "true" else "false"
      IntLiteralConstExpr i ->
        pure $ annotate IntLiteralAnn $ viaShow i
      CharLiteralConstExpr c ->
        pure $ annotate CharLiteralAnn $ viaShow c
      StringLiteralConstExpr s ->
        pure $ annotate StringLiteralAnn $ viaShow s

prettyRangeExpression :: Prettifier RangeExpression
prettyRangeExpression = \case
  RangeInclusiveExpr e1 e2 -> do
    lhs <- prettyExpression e1
    rhs <- prettyExpression e2
    pure $ parens lhs <+> "..=" <+> parens rhs
  RangeExclusiveExpr e1 e2 -> do
    lhs <- prettyExpression e1
    rhs <- prettyExpression e2
    pure $ parens lhs <+> ".." <+> parens rhs

prettyReturn :: Prettifier (Maybe (Typed Expression))
prettyReturn = \case
  Nothing -> pure $ annotate KeywordAnn "return" <> ";"
  Just e  -> do
    expr <- prettyExpression e
    pure $ annotate KeywordAnn "return" <+> expr <> ";"

prettyIf :: Prettifier IfInfo
prettyIf IfInfo {..} = do
  iexpr <- prettyExpression _ifExpr
  ibody <- nested $ prettyBlock _ifBody
  ebody <- case _ifElse of
    Nothing             -> pure []
    Just (ElseIf    ii) -> mkElse <$> prettyIf ii
    Just (ElseBlock  b) -> mkElse <$> nested (prettyBlock b)
  pure $ hsep $
    [ annotate KeywordAnn "if"
    , iexpr
    , ibody
    ] <> ebody
  where
    mkElse doc = [annotate KeywordAnn "else", doc]

prettyFor :: Prettifier ForInfo
prettyFor ForInfo {..} = do
  fexpr <- prettyRangeExpression _forRangeExpr
  nested do
    varID <- register $ mangle _forVariableName
    fbody <- prettyBlock _forBody
    pure $ hsep
      [ annotate KeywordAnn "for"
      , annotate (DeclarationAnn varID) (prettyIdentifier _forVariableName)
      , annotate KeywordAnn "in"
      , fexpr
      , fbody
      ]

prettyWhile :: Prettifier WhileInfo
prettyWhile WhileInfo {..} = do
  wexpr <- prettyExpression _whileExpr
  wbody <- nested $ prettyBlock _whileBody
  pure $ hsep
    [ annotate KeywordAnn "while"
    , wexpr
    , wbody
    ]

prettyLet :: Prettifier LetInfo
prettyLet LetInfo {..} = do
  ltype <- prettyConcreteType $ _typeInfo _letValue
  lexpr <- prettyExpression _letValue
  varID <- register $ mangle _letName
  pure $ annotate KeywordAnn "let"
    <+> annotate (DeclarationAnn varID) (prettyIdentifier _letName)
    <>  ":"
    <+> ltype
    <+> "="
    <+> lexpr
    <>  ";"

prettyStatement :: Prettifier Statement
prettyStatement = \case
  IfStmt         ii -> prettyIf ii
  ForStmt        fi -> prettyFor fi
  WhileStmt      wi -> prettyWhile wi
  LetStmt        li -> prettyLet li
  ReturnStmt     rs -> prettyReturn rs
  ExpressionStmt  e -> prettyExpression e <&> (<> ";")
  ContinueStmt      -> pure $ annotate KeywordAnn "continue" <> ";"
  BreakStmt         -> pure $ annotate KeywordAnn "break" <> ";"

prettyBlock :: Prettifier Block
prettyBlock block = do
  ps <- traverse (prettyStatement . _located) block
  pure $ vsep
    [ "{"
    , indent 2 $ vsep ps
    , "}"
    ]

prettyArg
  :: Identifier
  -> FunctionArgType ConcreteType
  -> Printer (Doc Annotation)
prettyArg argName argType = do
  varID <- register $ mangle argName
  typeDoc <- case argType of
    ByValue     innerType -> prettyConcreteType innerType
    ByReference innerType -> prettyConcreteType innerType <&> ("&" <>)
  let argDoc = annotate (DeclarationAnn varID) (prettyIdentifier argName)
  pure $ argDoc <> ":" <+> typeDoc

prettyEnum
  :: BaseName
  -> EnumInfo
  -> Printer (Doc Annotation)
prettyEnum baseName EnumInfo {..} = do
  enumID <- retrieve $ mangle baseName
  pure $ hsep
    [ annotate KeywordAnn "enum"
    , annotate (DeclarationAnn enumID) $ prettyIdentifier _enumName
    , encloseSep "{" "}" ", " $ map prettyIdentifier _enumValues
    ]

prettyStruct
  :: BaseName
  -> StructInfo ParameterizedFunctor
  -> Printer (Doc Annotation)
prettyStruct baseName StructInfo {..} = do
  structID <- retrieve $ mangle baseName
  prettifiedParams <- for _structParams \paramName -> do
    paramID <- register $ mangle paramName
    pure $ annotate (DeclarationAnn paramID) (prettyIdentifier paramName)
  prettifiedFields <- for (NE.toList _structValues) \(fieldName, fieldType) -> do
    prettifiedType <- prettyParameterizedType fieldType
    pure $ prettyIdentifier fieldName <> ":" <+> prettifiedType
  let
    structName = annotate (DeclarationAnn structID) (prettyIdentifier $ _nameIdent baseName)
    structDoc =
        if null _structParams
        then structName
        else structName <> "<" <> sepByCommas prettifiedParams <> ">"
  pure $ vsep
    [ annotate KeywordAnn "struct" <+> structDoc <+> "{"
    , indent 2 $ vsep prettifiedFields
    , "}"
    ]

prettyConst
  :: BaseName
  -> Typed ConstExpression
  -> Printer (Doc Annotation)
prettyConst baseName typedExpr = do
  constID <- retrieve $ mangle baseName
  constType <- prettyConcreteType $ _typeInfo typedExpr
  constExpr <- prettyConstExpression typedExpr
  pure $ annotate KeywordAnn "const"
    <+> annotate (DeclarationAnn constID) (prettyIdentifier $ _nameIdent baseName)
    <>  ":"
    <+> constType
    <+> "="
    <+> constExpr
    <>  ";"

prettyFunction
  :: Name
  -> FunctionInfo
  -> Printer (Doc Annotation)
prettyFunction (mangle -> name) FunctionInfo {..} = do
  let FunctionTypeInfo {..} = _funType
  functionID <- retrieve name
  args <- traverse (uncurry prettyArg) _funArgs
  returnType <- case _funReturn of
    UnitType -> pure Nothing
    _        -> Just <$> prettyConcreteType _funReturn
  body <- prettyBlock _funBody
  pure $ annotate KeywordAnn "fn"
    <+> annotate (DeclarationAnn functionID) (pretty name)
    <>  encloseSep "(" ")" ", " args
    <>  foldMap (" ->" <+>) returnType
    <+> body

prettySymbolCache :: Prettifier SymbolCache
prettySymbolCache cache = do
  M.forWithKey_ cache \name _ -> do
    register $ mangle name
  sepByNewlines . M.elems <$> M.traverseWithKey prettyFunction cache

prettyDefinitionCache :: Prettifier DefinitionCache
prettyDefinitionCache definitions = do
  M.forWithKey_ definitions \baseName _ -> do
    register $ mangle baseName
  sepByNewlines . catMaybes <$>
    for (M.toList definitions) \(definitionName, definition) ->
      case definition of
        EnumDef   info -> Just <$> prettyEnum   definitionName info
        StructDef info -> Just <$> prettyStruct definitionName info
        ConstDef  info -> Just <$> prettyConst  definitionName info
        _              -> pure Nothing

prettyInterface :: Prettifier Interface
prettyInterface Interface {..} = do
  defs <- prettyDefinitionCache _interfaceDefinitions
  syms <- prettySymbolCache _interfaceSymbols
  pure $ defs <> hardline <> hardline <> syms
