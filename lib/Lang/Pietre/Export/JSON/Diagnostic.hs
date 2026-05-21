module Lang.Pietre.Export.JSON.Diagnostic (serialize) where

import "this" Prelude

import Data.Aeson
import Data.Aeson.KeyMap                         qualified as KeyMap
import Data.Aeson.Types
import Data.Text                                 qualified as Text
import GHC.IsList                                (fromList)

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated hiding
                                                              (structBaseName)
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


--------------------------------------------------------------------------------
-- API

serialize :: (Functor t, Foldable t) => t Diagnostic -> Value
serialize = foldl' mergeValues baseObject . fmap visit
  where
    baseObject = object ["errors" .= emptyArray, "warnings" .= emptyArray]


--------------------------------------------------------------------------------
-- Internal

mergeValues :: Value -> Value -> Value
mergeValues = curry \case
  (Object lhs, Object rhs) -> Object $ KeyMap.unionWith mergeValues lhs rhs
  (Array  lhs, Array  rhs) -> Array  $ lhs <> rhs
  (lhs, Null) -> lhs
  (Null, rhs) -> rhs
  (lhs,  rhs) -> reportICE "Diagnostic.serialize" "incompatible JSON values" [show lhs, show rhs]

class Serialize a where
  visit :: a -> Value

instance (Serialize a) => Serialize (Maybe a) where
  visit = maybe Null visit

instance {-# OVERLAPPABLE #-} (Foldable t, Serialize a) => Serialize (t a) where
  visit = Array . fromList . map visit . toList

instance Serialize Diagnostic where
  visit Diagnostic {..} = case _diagnosticMessage of
    ErrorLexing                                                                          -> go "errors"   "Lexing"                               $ object []
    ErrorParsing                              actual expected                            -> go "errors"   "Parsing"                              $ object ["actual" .= visit actual, "expected" .= visitTokens expected]
    ErrorCircularImport                       moduleName path                            -> go "errors"   "CircularImport"                       $ object ["module name" .= visit moduleName, "path" .= visit path]
    ErrorFileNotFound                         path                                       -> go "errors"   "FileNotFound"                         $ object ["path" .= visit path]
    ErrorModuleNotFound                       moduleName includePaths                    -> go "errors"   "ModuleNotFound"                       $ object ["module name" .= visit moduleName, "include paths" .= visit includePaths]
    ErrorAmbiguousModule                      moduleName files                           -> go "errors"   "AmbiguousModule"                      $ object ["module name" .= visit moduleName, "files" .= visit files]
    ErrorImportSymbol                         moduleName identifier                      -> go "errors"   "ImportSymbol"                         $ object ["module name" .= visit moduleName, "identifier" .= visit identifier]
    ErrorMultipleDeclaration                  identifier declarations                    -> go "errors"   "MultipleDeclaration"                  $ object ["identifier" .= visit identifier, "declarations" .= visit declarations]
    ErrorRoleNotFound                         path                                       -> go "errors"   "RoleNotFound"                         $ object ["path" .= visit path]
    ErrorNotAType                             role                                       -> go "errors"   "NotAType"                             $ object ["role" .= visit role]
    ErrorNotAConst                            role                                       -> go "errors"   "NotAConst"                            $ object ["role" .= visit role]
    ErrorNotAStruct                           partialType                                -> go "errors"   "NotAStruct"                           $ object ["partial type" .= visit partialType]
    ErrorNotAValue                            role                                       -> go "errors"   "NotAValue"                            $ object ["role" .= visit role]
    ErrorNotAnLValue                          role                                       -> go "errors"   "NotAnLValue"                          $ object ["role" .= visit role]
    ErrorNotAFunctionRole                     role                                       -> go "errors"   "NotAFunctionRole"                     $ object ["role" .= visit role]
    ErrorNotAFunctionType                     concreteType                               -> go "errors"   "NotAFunctionType"                     $ object ["concrete type" .= visit concreteType]
    ErrorInvalidLValue                        expr                                       -> go "errors"   "InvalidLValue"                        $ object ["expr" .= visit expr]
    ErrorAmbiguousPath                        path roles                                 -> go "errors"   "AmbiguousPath"                        $ object ["path" .= visit path, "roles" .= visit roles]
    ErrorCyclicDefinition                     baseType typePath                          -> go "errors"   "CyclicDefinition"                     $ object ["base type" .= visit baseType, "type path" .= visit typePath]
    ErrorIncorrectTypeParameterCount          baseName expected actual                   -> go "errors"   "IncorrectTypeParameterCount"          $ object ["base name" .= visit baseName, "expected" .= visit expected, "actual" .= visit actual]
    ErrorDuplicatedTypeParameter              identifier                                 -> go "errors"   "DuplicatedTypeParameter"              $ object ["identifier" .= visit identifier]
    ErrorEnumDuplicatedEntry                  identifier                                 -> go "errors"   "EnumDuplicatedEntry"                  $ object ["identifier" .= visit identifier]
    ErrorWrongType                            expected actual                            -> go "errors"   "WrongType"                            $ object ["expected" .= visit expected, "actual" .= visit actual]
    ErrorIncompatibleType                     partialType concreteType                   -> go "errors"   "IncompatibleType"                     $ object ["partial type" .= visit partialType, "concrete type" .= visit concreteType]
    ErrorWrongCast                            typeFrom typeTo                            -> go "errors"   "WrongCast"                            $ object ["from" .= visit typeFrom, "to" .= visit typeTo]
    ErrorEnumOutOfBounds                      baseName index                             -> go "errors"   "EnumOutOfBounds"                      $ object ["base name" .= visit baseName, "index" .= visit index]
    ErrorStructMissingField                   structBaseName fieldName                   -> go "errors"   "StructMissingField"                   $ object ["base name" .= visit structBaseName, "field name" .= visit fieldName]
    ErrorStructDuplicatedField                structBaseName fieldName                   -> go "errors"   "StructDuplicatedField"                $ object ["base name" .= visit structBaseName, "field name" .= visit fieldName]
    ErrorStructUnknownField                   structBaseName fieldName                   -> go "errors"   "StructUnknownField"                   $ object ["base name" .= visit structBaseName, "field name" .= visit fieldName]
    ErrorStructAmbiguousType                  structBaseName parameterName               -> go "errors"   "StructAmbiguousType"                  $ object ["base name" .= visit structBaseName, "parameter name" .= visit parameterName]
    ErrorStructIncompatibleTypes              structBaseName typeParameter concreteTypes -> go "errors"   "StructIncompatibleTypes"              $ object ["base name" .= visit structBaseName, "type parameter" .= visit typeParameter, "concrete types" .= visit concreteTypes]
    ErrorFunctionAmbiguousType                functionBaseName parameterName             -> go "errors"   "FunctionAmbiguousType"                $ object ["base name" .= visit functionBaseName, "parameter name" .= visit parameterName]
    ErrorFunctionIncompatibleTypes            structBaseName typeParameter concreteTypes -> go "errors"   "FunctionIncompatibleTypes"            $ object ["base name" .= visit structBaseName, "type parameter" .= visit typeParameter, "concrete types" .= visit concreteTypes]
    ErrorFieldAccessNotAStruct                concreteType                               -> go "errors"   "FieldAccessNotAStruct"                $ object ["concrete type" .= visit concreteType]
    ErrorFieldAccessFieldNotFound             concreteType fieldName                     -> go "errors"   "FieldAccessFieldNotFound"             $ object ["concrete type" .= visit concreteType, "field name" .= visit fieldName]
    ErrorReservedIdentifier                   identifier                                 -> go "errors"   "ReservedIdentifier"                   $ object ["identifier" .= visit identifier]
    ErrorRootPlaceholder                                                                 -> go "errors"   "RootPlaceholder"                      $ object []
    ErrorInvalidPlaceholder                                                              -> go "errors"   "InvalidPlaceholder"                   $ object []
    ErrorFunctionDuplicatedArg                argName                                    -> go "errors"   "FunctionDuplicatedArg"                $ object ["arg name" .= visit argName]
    ErrorBreakNotInLoop                                                                  -> go "errors"   "BreakNotInLoop"                       $ object []
    ErrorContinueNotInLoop                                                               -> go "errors"   "ContinueNotInLoop"                    $ object []
    ErrorFunctionCallWrongNumberOfArguments   functionBaseName expected actual           -> go "errors"   "FunctionCallWrongNumberOfArguments"   $ object ["base name" .= visit functionBaseName, "expected" .= visit expected, "actual" .= visit actual]
    ErrorDivideByZero                                                                    -> go "errors"   "DivideByZero"                         $ object []
    ErrorNegativeExponent                                                                -> go "errors"   "NegativeExponent"                     $ object []
    ErrorReferenceNotLocalVariable            expr                                       -> go "errors"   "ReferenceNotLocalVariable"            $ object ["expr" .= visit expr]
    ErrorFunctionCallArgExpectingReference    functionBaseName argName expr              -> go "errors"   "FunctionCallArgExpectingReference"    $ object ["base name" .= visit functionBaseName, "arg name" .= visit argName, "expr" .= visit expr]
    ErrorFunctionCallArgNotExpectingReference functionBaseName argName                   -> go "errors"   "FunctionCallArgNotExpectingReference" $ object ["base name" .= visit functionBaseName, "arg name" .= visit argName]
    ErrorNoMainSymbol                                                                    -> go "errors"   "NoMainSymbol"                         $ object []
    ErrorSymbolNotFound                       symbolName                                 -> go "errors"   "SymbolNotFound"                       $ object ["symbol name" .= visit symbolName]
    WarningNameShadow                         shadowed identifier role                   -> go "warnings" "NameShadow"                           $ object ["shadowed" .= visit shadowed, "identifier" .= visit identifier, "role" .= visit role]
    WarningUnexpectedTopLevelExpression       expr                                       -> go "warnings" "UnexpectedTopLevelExpression"         $ object ["expr" .= visit expr]
    where
      go :: Key -> String -> Value -> Value
      go kind msg arguments =
        let
          innerObject = object
            [ "message"     .= msg
            , "arguments"   .= arguments
            , "location"    .= visit _diagnosticLocation
            , "declaration" .= visit _diagnosticDeclaration
            ]
        in
          object [kind .= [innerObject]]
      visitTokens :: [String] -> Value
      visitTokens = toJSON . map (filter (/= '"'))

instance Serialize Token where
  visit = \case
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
    TLiteralString s        -> String $ "string literal " <> Text.pack (show s)
    TLiteralChar   c        -> String $ "char literal "   <> Text.pack (show c)
    TLiteralInt    i        -> String $ "int literal "    <> Text.pack (show i)
    TIdentifier    n        -> String $ "identifier "     <> Text.pack (show n)
    TEOF                    -> "end of file"

instance Serialize Role where
  visit = \case
    BuiltinType          name   -> object ["kind" .= String "builtin type",      "name" .= visit name]
    Struct           baseName   -> object ["kind" .= String "struct",            "name" .= visit baseName]
    Enum             baseName   -> object ["kind" .= String "enum",              "name" .= visit baseName]
    Constant         baseName   -> object ["kind" .= String "constant",          "name" .= visit baseName]
    Function         baseName   -> object ["kind" .= String "function",          "name" .= visit baseName]
    TypeAlias        baseName   -> object ["kind" .= String "type alias",        "name" .= visit baseName]
    FunctionArgument  argName _ -> object ["kind" .= String "function argument", "name" .= visit argName]
    LetVariable       varName   -> object ["kind" .= String "let variable",      "name" .= visit varName]
    Placeholder                 -> object ["kind" .= String "placeholder"]
    TypeParameter baseName name -> object
      [ "kind" .= String "type parameter"
      , "name" .= visit name
      , "type" .= visit baseName
      ]

instance Serialize Validated.Expression where
  visit = \case
    Validated.LocalVariableExpr            {} -> "local variable"
    Validated.ReferenceArgumentExpr        {} -> "reference argument"
    Validated.IndexExpr                    {} -> "indexing"
    Validated.FunctionNameExpr             {} -> "function"
    Validated.FunctionCallExpr             {} -> "function call"
    Validated.VariableCallExpr             {} -> "function call"
    Validated.ArrayExpr                    {} -> "array"
    Validated.StructExpr                   {} -> "struct"
    Validated.FieldAccessExpr              {} -> "field access"
    Validated.BoolLiteralExpr              {} -> "literal"
    Validated.IntLiteralExpr               {} -> "literal"
    Validated.CharLiteralExpr              {} -> "literal"
    Validated.StringLiteralExpr            {} -> "literal"
    Validated.IntNegationExpr              {} -> "boolean"
    Validated.BoolNegationExpr             {} -> "boolean"
    Validated.AdditionExpr                 {} -> "arithmetic"
    Validated.SubtractionExpr              {} -> "arithmetic"
    Validated.MultiplicationExpr           {} -> "arithmetic"
    Validated.DivisionExpr                 {} -> "arithmetic"
    Validated.ModuloExpr                   {} -> "arithmetic"
    Validated.ExponentiationExpr           {} -> "arithmetic"
    Validated.EqualityExpr                 {} -> "comparison"
    Validated.DifferenceExpr               {} -> "comparison"
    Validated.GreaterExpr                  {} -> "comparison"
    Validated.LesserExpr                   {} -> "comparison"
    Validated.GreaterEqExpr                {} -> "comparison"
    Validated.LesserEqExpr                 {} -> "comparison"
    Validated.BoolAndExpr                  {} -> "boolean"
    Validated.BoolOrExpr                   {} -> "boolean"
    Validated.CastExpr                     {} -> "cast"
    Validated.RangeExpr                    {} -> "range"
    Validated.AssignmentExpr               {} -> "assignment"
    Validated.AdditionAssignmentExpr       {} -> "assignment"
    Validated.SubtractionAssignmentExpr    {} -> "assignment"
    Validated.MultiplicationAssignmentExpr {} -> "assignment"
    Validated.DivisionAssignmentExpr       {} -> "assignment"
    Validated.ModuloAssignmentExpr         {} -> "assignment"
    Validated.ExponentiationAssignmentExpr {} -> "assignment"

instance Serialize Resolved.Expression where
  visit = \case
    Resolved.PathExpr                     {} -> "path"
    Resolved.FieldAccessExpr              {} -> "field access"
    Resolved.CallExpr                     {} -> "function call"
    Resolved.ArrayExpr                    {} -> "array"
    Resolved.IndexExpr                    {} -> "indexing"
    Resolved.StructExpr                   {} -> "struct"
    Resolved.BoolLiteralExpr              {} -> "literal"
    Resolved.IntLiteralExpr               {} -> "literal"
    Resolved.CharLiteralExpr              {} -> "literal"
    Resolved.StringLiteralExpr            {} -> "literal"
    Resolved.ReferenceExpr                {} -> "reference"
    Resolved.IntNegationExpr              {} -> "boolean"
    Resolved.BoolNegationExpr             {} -> "boolean"
    Resolved.AdditionExpr                 {} -> "arithmetic"
    Resolved.SubtractionExpr              {} -> "arithmetic"
    Resolved.MultiplicationExpr           {} -> "arithmetic"
    Resolved.DivisionExpr                 {} -> "arithmetic"
    Resolved.ModuloExpr                   {} -> "arithmetic"
    Resolved.ExponentiationExpr           {} -> "arithmetic"
    Resolved.EqualityExpr                 {} -> "comparison"
    Resolved.DifferenceExpr               {} -> "comparison"
    Resolved.GreaterExpr                  {} -> "comparison"
    Resolved.LesserExpr                   {} -> "comparison"
    Resolved.GreaterEqExpr                {} -> "comparison"
    Resolved.LesserEqExpr                 {} -> "comparison"
    Resolved.BoolAndExpr                  {} -> "boolean"
    Resolved.BoolOrExpr                   {} -> "boolean"
    Resolved.CastExpr                     {} -> "cast"
    Resolved.RangeInclusiveExpr           {} -> "range"
    Resolved.RangeExclusiveExpr           {} -> "range"
    Resolved.AssignmentExpr               {} -> "assignment"
    Resolved.AdditionAssignmentExpr       {} -> "assignment"
    Resolved.SubtractionAssignmentExpr    {} -> "assignment"
    Resolved.MultiplicationAssignmentExpr {} -> "assignment"
    Resolved.DivisionAssignmentExpr       {} -> "assignment"
    Resolved.ModuloAssignmentExpr         {} -> "assignment"
    Resolved.ExponentiationAssignmentExpr {} -> "assignment"


instance {-# OVERLAPPING #-} Serialize (Maybe (TypeNode Maybe)) where
  visit = visitTypeTree (maybe $ object ["kind" .= String "placeholder"])

instance Serialize (TypeNode Identity) where
  visit = visitTypeTree id

visitTypeTree
  :: ((TypeNode f -> Value) -> TypeTree f -> Value)
  -> TypeTree f
  -> Value
visitTypeTree f = f go
  where
    go = \case
      IntType      -> object ["kind" .= String "scalar", "name" .= String "int"   ]
      BoolType     -> object ["kind" .= String "scalar", "name" .= String "bool"  ]
      CharType     -> object ["kind" .= String "scalar", "name" .= String "char"  ]
      UnitType     -> object ["kind" .= String "scalar", "name" .= String "()"    ]
      VoidType     -> object ["kind" .= String "error",  "name" .= String "!void" ]
      EnumType n _ -> object ["kind" .= String "enum",   "name" .= visit n ]
      StructType StructTypeInfo{..} -> object
        [ "kind"       .= String "struct"
        , "name"       .= visit _structBaseName
        , "parameters" .= toJSON (map (f go) _structTypeParams)
        ]
      Validated.FunctionType FunctionTypeInfo{..} -> object
        [ "kind"        .= String "function"
        , "arguments"   .= toJSON (map visitArgs _funArgs)
        , "return type" .= f go _funReturn
        ]
    visitArgs = snd >>> \case
      ByValue     t -> object ["type" .= f go t, "kind" .= String "value"]
      ByReference t -> object ["type" .= f go t, "kind" .= String "reference"]

instance Serialize Name where
  visit Name {..} = object
    [ "module"     .= visit (_nameModule _nameBase)
    , "identifier" .= visit (_nameIdent  _nameBase)
    , "parameters" .= visit _nameParams
    ]

instance Serialize BaseName where
  visit BaseName {..} = object
    [ "module"     .= visit _nameModule
    , "identifier" .= visit _nameIdent
    ]

instance Serialize Location where
  visit Location {..} = object
    [ "filename" .= _locFilename
    , "line"     .= _locLine
    , "column"   .= _locColumn
    ]

instance Serialize Identifier where
  visit = String . rawIdentifier

instance Serialize Int where
  visit = Number . fromIntegral

instance Serialize Bool where
  visit = Bool

instance Serialize Char where
  visit = String . Text.pack . pure
