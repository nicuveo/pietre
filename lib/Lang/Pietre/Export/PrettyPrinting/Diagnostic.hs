module Lang.Pietre.Export.PrettyPrinting.Diagnostic (prettyPrint) where

import "this" Prelude

import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated hiding
                                                              (structBaseName)
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


--------------------------------------------------------------------------------
-- API

prettyPrint :: Diagnostic -> Text
prettyPrint = prettyDiagnostic
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> renderStrict


--------------------------------------------------------------------------------
-- Internal helpers

prettyDiagnostic :: Diagnostic -> Doc ()
prettyDiagnostic Diagnostic {..} =
  prettyDiagnosticLocation _diagnosticLocation <+>
  prettyMessage _diagnosticDeclaration _diagnosticMessage

prettyDiagnosticLocation :: Maybe Location -> Doc ()
prettyDiagnosticLocation = maybe "<no location info>" \Location {..} -> concatWith (surround ":")
  [ pretty _locFilename
  , pretty _locLine
  , pretty _locColumn
  ] <> ":"

prettyMessage :: Maybe BaseName -> Message -> Doc ()
prettyMessage decl = \case
  ErrorLexing ->
    goError "lexing error"
  ErrorParsing actual [] ->
    goError $ hsep
      [ "parse error: unexpected token:"
      , prettyToken actual
      ]
  ErrorParsing actual expected ->
    goError $ hsep
      [ "parse error: expecting one of"
      , list (map pretty expected)
      , "but found token:"
      , prettyToken actual
      ]
  ErrorCircularImport moduleName path ->
    let
      header        = "module graph forms a cycle:"
      firstModule   = "        module" <+> prettyModuleName moduleName
      otherModule m = "imports module" <+> prettyModuleName m
      moduleList    = map otherModule $ toList path
    in
      goError $ vsep $ header : indentList (firstModule : moduleList)
  ErrorFileNotFound path ->
    goError $ "file not found:" <+> pretty path
  ErrorModuleNotFound moduleName includePaths ->
    let
      header  = "module not found:" <+> prettyModuleName moduleName
      folders = ["not found in:" <+> pretty path | path <- toList includePaths]
    in
      goError $ vsep $ header : indentList folders
  ErrorAmbiguousModule moduleName files ->
    let
      header  = "ambiguous file for module:" <+> prettyModuleName moduleName
      matches = ["name matches:" <+> pretty path | path <- toList files]
    in
      goError $ vsep $ header : indentList matches
  ErrorImportSymbol moduleName identifier ->
    goError $ hsep
      [ "module"
      , prettyModuleName moduleName
      , "does not export symbol"
      , prettyIdentifier identifier
      ]
  ErrorMultipleDeclaration identifier declarations ->
    let
      header    = hsep ["symbol", prettyIdentifier identifier, "declared in multiple locations:"]
      locations = ["at" <+> prettyLocation l | l <- toList declarations]
    in
      goError $ vsep $ header : indentList locations
  ErrorRoleNotFound path ->
    goError $ hsep
      [ "identifier"
      , prettyPath path
      , "not found in scope"
      ]
  ErrorNotAType role ->
    goError $ "expected a type, but found:" <+> prettyRole role
  ErrorNotAConst role ->
    goError $ "expected a constant value, but found:" <+> prettyRole role
  ErrorNotAStruct partialType ->
    goError $ "expected a struct, but found:" <+> prettyPartialTypeName partialType
  ErrorNotAValue role ->
    goError $ "expected a value, but found:" <+> prettyRole role
  ErrorNotAnLValue role ->
    goError $ "expected an lvalue, but found:" <+> prettyRole role
  ErrorNotAFunctionRole role ->
    goError $ "expected a function, but found:" <+> prettyRole role
  ErrorNotAFunctionType concreteType ->
    goError $ "expected a function type, but found:" <+> prettyConcreteType concreteType
  ErrorInvalidLValue expr ->
    goError $ "expected an lvalue, but found:" <+> prettyResolvedExprName expr
  ErrorAmbiguousPath path roles ->
    let
      header = hsep ["identifier", prettyPath path, "ambiguous in scope; did you mean:"]
    in
      goError $ vsep $ header : indentList (map prettyRole $ toList roles)
  ErrorCyclicDefinition baseType typePath ->
    let
      header      = "definition forms a cycle:"
      firstType   = "           type" <+> prettyBaseName baseType
      otherType t = "depends on type" <+> prettyBaseName t
      typeList    = map otherType $ reverse typePath
    in
      goError $ vsep $ header : indentList (firstType : typeList)
  ErrorIncorrectTypeParameterCount baseName expected actual ->
    goError $ "incorrect parameter count for type"
      <+> prettyBaseName baseName
      <>  "; expected"
      <+> pretty expected
      <+> "but got:"
      <+> pretty actual
  ErrorDuplicatedTypeParameter identifier ->
    goError $ hsep ["duplicate type parameter name:", prettyIdentifier identifier]
  ErrorEnumDuplicatedEntry identifier ->
    goError $ hsep ["duplicate enum value:", prettyIdentifier identifier]
  ErrorWrongType expected actual ->
    let
      header = "unexpected type:" <+> prettyConcreteType actual <> "; expecting one of:"
      types  = map prettyConcreteType expected
    in
      goError $ vsep $ header : indentList types
  ErrorIncompatibleType partialType concreteType ->
    goError $ hsep
      [ "found type"
      , prettyConcreteType concreteType
      , "but was expecting"
      , prettyPartialType partialType
      ]
  ErrorWrongCast typeFrom typeTo ->
    goError $ hsep
      [ "invalid cast from"
      , prettyConcreteType typeFrom
      , "to"
      , prettyConcreteType typeTo
      ]
  ErrorEnumOutOfBounds baseName index ->
    goError $ hsep [pretty index, "is not a valid value for", prettyBaseName baseName]
  ErrorStructMissingField structBaseName fieldName ->
    goError $ hsep
      [ "struct"
      , prettyBaseName structBaseName
      , "is missing field"
      , prettyIdentifier fieldName
      ]
  ErrorStructDuplicatedField structBaseName fieldName ->
    goError $ hsep
      [ "struct"
      , prettyBaseName structBaseName
      , "has more than one entry for field"
      , prettyIdentifier fieldName
      ]
  ErrorStructUnknownField structBaseName fieldName ->
    goError $ hsep
      [ "struct"
      , prettyBaseName structBaseName
      , "has no field named"
      , prettyIdentifier fieldName
      ]
  ErrorStructAmbiguousType structBaseName parameterName ->
    goError $ hsep
      [ "could not determine type for parameter"
      , prettyIdentifier parameterName
      , "of struct"
      , prettyBaseName structBaseName
      ]
  ErrorStructIncompatibleTypes structBaseName typeParameter concreteTypes ->
    let
      header = hsep
        [ "incompatible types for parameter"
        , prettyIdentifier typeParameter
        , "of struct"
        , prettyBaseName structBaseName
        ]
      types = ["found:" <+> prettyConcreteType t | t <- concreteTypes]
    in
      goError $ vsep $ header : indentList types
  ErrorFunctionAmbiguousType functionBaseName parameterName ->
    goError $ hsep
      [ "could not determine type for parameter"
      , prettyIdentifier parameterName
      , "of function"
      , prettyBaseName functionBaseName
      ]
  ErrorFunctionIncompatibleTypes structBaseName typeParameter concreteTypes ->
    let
      header = hsep
        [ "incompatible types for parameter"
        , prettyIdentifier typeParameter
        , "of function"
        , prettyBaseName structBaseName
        ]
      types = ["found:" <+> prettyConcreteType t | t <- concreteTypes]
    in
      goError $ vsep $ header : indentList types
  ErrorFieldAccessNotAStruct concreteType ->
    goError $
      "left-hand side of field access is expecting a struct, but got " <+>
      prettyConcreteType concreteType
  ErrorFieldAccessFieldNotFound concreteType fieldName ->
    goError $ hsep
      [ prettyConcreteType concreteType
      , "does not have a field named"
      , prettyIdentifier fieldName
      ]
  ErrorReservedIdentifier identifier ->
    goError $ prettyIdentifier identifier <+> "is a reserved identifier"
  ErrorRootPlaceholder ->
    goError "expected partial type, found placeholder"
  ErrorInvalidPlaceholder ->
    goError "unexpected placeholder in type name"
  ErrorFunctionDuplicatedArg argName ->
    goError $ "duplicate argument name:" <+> prettyIdentifier argName
  ErrorBreakNotInLoop ->
    goError "break statement outside of a loop"
  ErrorContinueNotInLoop ->
    goError "continue statement outside of a loop"
  ErrorFunctionCallWrongNumberOfArguments functionBaseName expected actual ->
    goError $ "incorrect number of arguments for function"
      <+> prettyBaseName functionBaseName
      <>  "; expecting"
      <+> pretty expected
      <+> "but found"
      <+> pretty actual
  ErrorDivideByZero ->
    goError "division by zero"
  ErrorNegativeExponent ->
    goError "negative exponent"
  ErrorReferenceNotLocalVariable expr ->
    goError $ "cannot create reference from" <+> prettyValidatedExprName expr
  ErrorFunctionCallArgExpectingReference functionBaseName argName expr ->
    goError $ hsep
      [ "argument"
      , prettyIdentifier argName
      , "of function"
      , prettyBaseName functionBaseName
      , "expects a reference, but got"
      , prettyResolvedExprName expr
      ]
  ErrorFunctionCallArgNotExpectingReference functionBaseName argName ->
    goError $ hsep
      [ "argument"
      , prettyIdentifier argName
      , "of function"
      , prettyBaseName functionBaseName
      , "does not expect a reference"
      ]
  ErrorNoMainSymbol ->
    goError "could not find required function main"
  ErrorSymbolNotFound symbolName ->
    goError $ "link symbol not found:" <+> prettyName symbolName
  WarningNameShadow shadowed identifier role ->
    let
      header = hsep
        [ prettyRole role
        , prettyIdentifier identifier
        , "shadows existing declarations:"
        ]
      decls = map prettyRole $ toList shadowed
    in
      goWarning $ vsep $ header : indentList decls
  WarningUnexpectedTopLevelExpression expr ->
    goWarning $ "expected statement but found:" <+> prettyValidatedExprName expr
  where
    indentList = map (indent 4)
    goError    = go "error:"
    goWarning  = go "warning:"
    goDecl baseName = "in declaration of" <+> prettyBaseName baseName <> ":"
    go dtype msg = dtype <> hardline <> indent 4 (vsep $ catMaybes [fmap goDecl decl, Just msg])

prettyToken :: Token -> Doc ()
prettyToken = \case
  TKeywordAs              -> "keyword 'as'"
  TKeywordBreak           -> "keyword 'break'"
  TKeywordConst           -> "keyword 'const'"
  TKeywordContinue        -> "keyword 'continue'"
  TKeywordElse            -> "keyword 'else'"
  TKeywordEnum            -> "keyword 'enum'"
  TKeywordFalse           -> "keyword 'false'"
  TKeywordFn              -> "keyword 'fn'"
  TKeywordFor             -> "keyword 'for'"
  TKeywordIf              -> "keyword 'if'"
  TKeywordIn              -> "keyword 'in'"
  TKeywordLet             -> "keyword 'let'"
  TKeywordReturn          -> "keyword 'return'"
  TKeywordStruct          -> "keyword 'struct'"
  TKeywordTrue            -> "keyword 'true'"
  TKeywordType            -> "keyword 'type'"
  TKeywordUse             -> "keyword 'use'"
  TKeywordWhile           -> "keyword 'while'"
  TOperatorAt             -> "operator '@'"
  TOperatorSemicolon      -> "operator ';'"
  TOperatorType           -> "operator '::'"
  TOperatorStar           -> "operator '*'"
  TOperatorComma          -> "operator ','"
  TOperatorAssign         -> "operator '='"
  TOperatorColon          -> "operator '@'"
  TOperatorArrow          -> "operator '->'"
  TOperatorLessThan       -> "operator '<'"
  TOperatorGreaterThan    -> "operator '>'"
  TOperatorDot            -> "operator '.'"
  TOperatorRangeInclusive -> "operator '..='"
  TOperatorRangeExclusive -> "operator '..'"
  TOperatorReference      -> "operator '&'"
  TOperatorNot            -> "operator '!'"
  TOperatorMinus          -> "operator '-'"
  TOperatorPlus           -> "operator '+'"
  TOperatorDiv            -> "operator '/'"
  TOperatorMod            -> "operator '%'"
  TOperatorPow            -> "operator '^'"
  TOperatorEqual          -> "operator '=='"
  TOperatorDiff           -> "operator '!='"
  TOperatorGreaterOrEqual -> "operator '>='"
  TOperatorLessOrEqual    -> "operator '<='"
  TOperatorBoolAnd        -> "operator '&&'"
  TOperatorBoolOr         -> "operator '||'"
  TOperatorAssignPlus     -> "operator '+='"
  TOperatorAssignMinus    -> "operator '-='"
  TOperatorAssignMult     -> "operator '*='"
  TOperatorAssignDiv      -> "operator '/='"
  TOperatorAssignMod      -> "operator '%='"
  TOperatorAssignPow      -> "operator '^='"
  TDelimiterParensOpen    -> "delimiter '('"
  TDelimiterParensClose   -> "delimiter ')'"
  TDelimiterBracesOpen    -> "delimiter '{'"
  TDelimiterBracesClose   -> "delimiter '}'"
  TDelimiterBracketsOpen  -> "delimiter '['"
  TDelimiterBracketsClose -> "delimiter ']'"
  TLiteralString s        -> "string literal '" <> pretty s <> "'"
  TLiteralChar   c        -> "char literal '"   <> pretty c <> "'"
  TLiteralInt    i        -> "int literal '"    <> pretty i <> "'"
  TIdentifier    n        -> "identifier '"     <> prettyIdentifier n <> "'"
  TEOF                    -> "end of file"

prettyIdentifier :: Identifier -> Doc ()
prettyIdentifier = pretty . rawIdentifier

prettyLocation :: Location -> Doc ()
prettyLocation Location {..} =
  concatWith (surround ":")
    [ pretty _locFilename
    , pretty _locLine
    , pretty _locColumn
    ]

prettyName :: Name -> Doc ()
prettyName Name {..} = case _nameParams of
  [] -> prettyBaseName _nameBase
  ps -> prettyBaseName _nameBase <> encloseSep "<" ">" "," (map prettyName ps)

prettyBaseName :: BaseName -> Doc ()
prettyBaseName BaseName {..} = hcat
  [ prettyModuleName _nameModule
  , "::"
  , prettyIdentifier _nameIdent
  ]

prettyModuleName :: ModuleName -> Doc ()
prettyModuleName = concatWith (surround "::") . map prettyIdentifier . toList

prettyPath :: Path -> Doc ()
prettyPath = concatWith (surround "::") . map prettyIdentifier . toList

prettyRole :: Role -> Doc ()
prettyRole = \case
  BuiltinType name ->
    "the builtin type" <+> prettyName name
  Struct baseName ->
    "the struct" <+> prettyBaseName baseName
  Enum baseName ->
    "the enum" <+> prettyBaseName baseName
  Constant baseName ->
    "the constant" <+> prettyBaseName baseName
  Function baseName ->
    "the function" <+> prettyBaseName baseName
  TypeAlias baseName ->
    "the type alias" <+> prettyBaseName baseName
  TypeParameter baseName identifier -> hsep
    [ "the type parameter"
    , prettyIdentifier identifier
    , "of"
    , prettyBaseName baseName
    ]
  Placeholder ->
    "a placeholder"
  FunctionArgument argName _ ->
    "the function argument" <+> prettyIdentifier argName
  LetVariable varName ->
    "the let variable" <+> prettyIdentifier varName

prettyPartialTypeName :: PartialType -> Doc ()
prettyPartialTypeName = maybe "a placeholder" \case
  IntType                  -> "int"
  BoolType                 -> "bool"
  CharType                 -> "char"
  UnitType                 -> "()"
  VoidType                 -> "!void"
  EnumType _ _             -> "an enum"
  StructType _             -> "a struct"
  Validated.FunctionType _ -> "a function type"

prettyPartialType :: PartialType -> Doc ()
prettyPartialType = prettyTypeTree (maybe "_")

prettyConcreteType :: ConcreteType -> Doc ()
prettyConcreteType = prettyTypeTree id

prettyTypeTree
  :: ((TypeNode f -> Doc ()) -> TypeTree f -> Doc ())
  -> TypeTree f
  -> Doc ()
prettyTypeTree f = f go
  where
    go = \case
      IntType  ->
        "int"
      BoolType ->
        "bool"
      CharType ->
        "char"
      UnitType ->
        "()"
      VoidType ->
        "!void"
      EnumType baseName _ ->
        prettyBaseName baseName
      StructType StructTypeInfo{..} ->
        case _structTypeParams of
          [] -> prettyBaseName _structBaseName
          ps -> prettyBaseName _structBaseName <> encloseSep "<" ">" "," (map (f go) ps)
      Validated.FunctionType FunctionTypeInfo{..} ->
        encloseSep "(" ")" "," (map (functionArg . snd) _funArgs) <+> "->" <+> f go _funReturn
    functionArg = \case
      ByValue     t ->        f go t
      ByReference t -> "&" <> f go t

prettyValidatedExprName :: Validated.Expression -> Doc ()
prettyValidatedExprName = \case
  Validated.LocalVariableExpr            _     -> "a local variable"
  Validated.ReferenceArgumentExpr        _     -> "a reference argument"
  Validated.IndexExpr                    _ _   -> "an indexing expression"
  Validated.FunctionNameExpr             _ _   -> "a function"
  Validated.FunctionCallExpr             _ _ _ -> "a function call"
  Validated.VariableCallExpr             _ _ _ -> "a function call"
  Validated.ArrayExpr                    _     -> "an array expression"
  Validated.StructExpr                   _ _   -> "a struct expression"
  Validated.FieldAccessExpr              _ _ _ -> "a field access expression"
  Validated.BoolLiteralExpr              _     -> "a literal"
  Validated.IntLiteralExpr               _     -> "a literal"
  Validated.CharLiteralExpr              _     -> "a literal"
  Validated.StringLiteralExpr            _     -> "a literal"
  Validated.IntNegationExpr              _     -> "a boolean expression"
  Validated.BoolNegationExpr             _     -> "a boolean expression"
  Validated.AdditionExpr                 _ _   -> "an arithmetic expression"
  Validated.SubtractionExpr              _ _   -> "an arithmetic expression"
  Validated.MultiplicationExpr           _ _   -> "an arithmetic expression"
  Validated.DivisionExpr                 _ _   -> "an arithmetic expression"
  Validated.ModuloExpr                   _ _   -> "an arithmetic expression"
  Validated.ExponentiationExpr           _ _   -> "an arithmetic expression"
  Validated.EqualityExpr                 _ _   -> "a comparison expression"
  Validated.DifferenceExpr               _ _   -> "a comparison expression"
  Validated.GreaterExpr                  _ _   -> "a comparison expression"
  Validated.LesserExpr                   _ _   -> "a comparison expression"
  Validated.GreaterEqExpr                _ _   -> "a comparison expression"
  Validated.LesserEqExpr                 _ _   -> "a comparison expression"
  Validated.BoolAndExpr                  _ _   -> "a boolean expression"
  Validated.BoolOrExpr                   _ _   -> "a boolean expression"
  Validated.CastExpr                     _ _   -> "a cast expression"
  Validated.RangeExpr                    _     -> "a range expression"
  Validated.AssignmentExpr               _ _   -> "an assignment expression"
  Validated.AdditionAssignmentExpr       _ _   -> "an assignment expression"
  Validated.SubtractionAssignmentExpr    _ _   -> "an assignment expression"
  Validated.MultiplicationAssignmentExpr _ _   -> "an assignment expression"
  Validated.DivisionAssignmentExpr       _ _   -> "an assignment expression"
  Validated.ModuloAssignmentExpr         _ _   -> "an assignment expression"
  Validated.ExponentiationAssignmentExpr _ _   -> "an assignment expression"

prettyResolvedExprName :: Resolved.Expression -> Doc ()
prettyResolvedExprName = \case
  Resolved.PathExpr                     _   -> "a path expression"
  Resolved.FieldAccessExpr              _ _ -> "a field access expression"
  Resolved.CallExpr                     _ _ -> "a function call expression"
  Resolved.ArrayExpr                    _   -> "an array expression"
  Resolved.IndexExpr                    _ _ -> "an indexing expression"
  Resolved.StructExpr                   _ _ -> "a struct expression"
  Resolved.BoolLiteralExpr              _   -> "a literal"
  Resolved.IntLiteralExpr               _   -> "a literal"
  Resolved.CharLiteralExpr              _   -> "a literal"
  Resolved.StringLiteralExpr            _   -> "a literal"
  Resolved.ReferenceExpr                _   -> "a reference expression"
  Resolved.IntNegationExpr              _   -> "a boolean expression"
  Resolved.BoolNegationExpr             _   -> "a boolean expression"
  Resolved.AdditionExpr                 _ _ -> "an arithmetic expression"
  Resolved.SubtractionExpr              _ _ -> "an arithmetic expression"
  Resolved.MultiplicationExpr           _ _ -> "an arithmetic expression"
  Resolved.DivisionExpr                 _ _ -> "an arithmetic expression"
  Resolved.ModuloExpr                   _ _ -> "an arithmetic expression"
  Resolved.ExponentiationExpr           _ _ -> "an arithmetic expression"
  Resolved.EqualityExpr                 _ _ -> "a comparison expression"
  Resolved.DifferenceExpr               _ _ -> "a comparison expression"
  Resolved.GreaterExpr                  _ _ -> "a comparison expression"
  Resolved.LesserExpr                   _ _ -> "a comparison expression"
  Resolved.GreaterEqExpr                _ _ -> "a comparison expression"
  Resolved.LesserEqExpr                 _ _ -> "a comparison expression"
  Resolved.BoolAndExpr                  _ _ -> "a boolean expression"
  Resolved.BoolOrExpr                   _ _ -> "a boolean expression"
  Resolved.CastExpr                     _ _ -> "a cast expression"
  Resolved.RangeInclusiveExpr           _ _ -> "a range expression"
  Resolved.RangeExclusiveExpr           _ _ -> "a range expression"
  Resolved.AssignmentExpr               _ _ -> "an assignment expression"
  Resolved.AdditionAssignmentExpr       _ _ -> "an assignment expression"
  Resolved.SubtractionAssignmentExpr    _ _ -> "an assignment expression"
  Resolved.MultiplicationAssignmentExpr _ _ -> "an assignment expression"
  Resolved.DivisionAssignmentExpr       _ _ -> "an assignment expression"
  Resolved.ModuloAssignmentExpr         _ _ -> "an assignment expression"
  Resolved.ExponentiationAssignmentExpr _ _ -> "an assignment expression"
