{
module Lang.Pietre.Stages.Parsing.Parser where

import "this" Prelude

import Control.Lens (over)
import Data.List.NonEmpty ((<|), singleton)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad

}


%name moduleParser module
%name expressionParser expression
%tokentype { (Location, Token) }

%error { happyError }
%errorhandlertype explist

%monad { Parser } { >>= } { return }
%lexer { lexer } { (_, TEOF) }

%right "=" "+=" "-=" "*=" "/=" "%=" "^="
%nonassoc "..=" ".."
%left "||"
%left "&&"
%nonassoc "<=" "<" ">=" ">" "==" "!="
%left "+" "-"
%left "*" "/" "%"
%left "^"
%left "as"
%left UNARY
%left "." "["

%token

"as"       { ($$, TKeywordAs)              }
"break"    { ($$, TKeywordBreak)           }
"const"    { ($$, TKeywordConst)           }
"continue" { ($$, TKeywordContinue)        }
"else"     { ($$, TKeywordElse)            }
"enum"     { ($$, TKeywordEnum)            }
"false"    { ($$, TKeywordFalse)           }
"fn"       { ($$, TKeywordFn)              }
"for"      { ($$, TKeywordFor)             }
"if"       { ($$, TKeywordIf)              }
"in"       { ($$, TKeywordIn)              }
"let"      { ($$, TKeywordLet)             }
"return"   { ($$, TKeywordReturn)          }
"struct"   { ($$, TKeywordStruct)          }
"true"     { ($$, TKeywordTrue)            }
"type"     { ($$, TKeywordType)            }
"use"      { ($$, TKeywordUse)             }
"while"    { ($$, TKeywordWhile)           }
"@"        { ($$, TOperatorAt)             }
";"        { ($$, TOperatorSemicolon)      }
"::"       { ($$, TOperatorType)           }
"*"        { ($$, TOperatorStar)           }
","        { ($$, TOperatorComma)          }
"="        { ($$, TOperatorAssign)         }
":"        { ($$, TOperatorColon)          }
"->"       { ($$, TOperatorArrow)          }
"<"        { ($$, TOperatorLessThan)       }
">"        { ($$, TOperatorGreaterThan)    }
"."        { ($$, TOperatorDot)            }
"..="      { ($$, TOperatorRangeInclusive) }
".."       { ($$, TOperatorRangeExclusive) }
"&"        { ($$, TOperatorReference)      }
"!"        { ($$, TOperatorNot)            }
"-"        { ($$, TOperatorMinus)          }
"+"        { ($$, TOperatorPlus)           }
"/"        { ($$, TOperatorDiv)            }
"%"        { ($$, TOperatorMod)            }
"^"        { ($$, TOperatorPow)            }
"=="       { ($$, TOperatorEqual)          }
"!="       { ($$, TOperatorDiff)           }
">="       { ($$, TOperatorGreaterOrEqual) }
"<="       { ($$, TOperatorLessOrEqual)    }
"&&"       { ($$, TOperatorBoolAnd)        }
"||"       { ($$, TOperatorBoolOr)         }
"+="       { ($$, TOperatorAssignPlus)     }
"-="       { ($$, TOperatorAssignMinus)    }
"*="       { ($$, TOperatorAssignMult)     }
"/="       { ($$, TOperatorAssignDiv)      }
"%="       { ($$, TOperatorAssignMod)      }
"^="       { ($$, TOperatorAssignPow)      }
"("        { ($$, TDelimiterParensOpen)    }
")"        { ($$, TDelimiterParensClose)   }
"{"        { ($$, TDelimiterBracesOpen)    }
"}"        { ($$, TDelimiterBracesClose)   }
"["        { ($$, TDelimiterBracketsOpen)  }
"]"        { ($$, TDelimiterBracketsClose) }

INT        { (_, TLiteralInt    _) }
CHAR       { (_, TLiteralChar   _) }
STRING     { (_, TLiteralString _) }
IDENTIFIER { (_, TIdentifier    _) }

%%

module :: { Module }
  : many(declaration) { mconcat $1 }

declaration :: { Module }
  : use_decl    { Module [$1] [] }
  | alias_decl  { Module [] [$1] }
  | enum_decl   { Module [] [$1] }
  | struct_decl { Module [] [$1] }
  | const_decl  { Module [] [$1] }
  | fun_decl    { Module [] [$1] }


use_decl :: { Import }
  : "use" use_tree ";" { $2 }

use_tree :: { Import }
  : IDENTIFIER optional(use_alias)                 { Import (singleton $ getIdentifierLiteral $1) (Qualified $2) }
  | IDENTIFIER "::" "*"                            { Import (singleton $ getIdentifierLiteral $1) Exhaustive }
  | IDENTIFIER "::" "{" comma_list(IDENTIFIER) "}" { Import (singleton $ getIdentifierLiteral $1) (Specific (NE.fromList $ map getIdentifierLiteral $4)) }
  | IDENTIFIER "::" use_tree                       { prependImport (getIdentifierLiteral $1) $3 }

use_alias :: { Identifier }
  : "as" IDENTIFIER { getIdentifierLiteral $2 }


alias_decl :: { WithLocation (Definition Parsed) }
  : "type" IDENTIFIER optional(generic_params) "=" type_expr ";" { WithLocation $1 (TypeAliasDef (TypeAliasInfo (getIdentifierLiteral $2) (fold $3) $5)) }

enum_decl :: { WithLocation (Definition Parsed) }
  : "enum" IDENTIFIER "{" optional(comma_list(enum_item)) "}" { WithLocation $1 (EnumDef (EnumInfo (getIdentifierLiteral $2) (fold $4))) }

enum_item :: { Identifier }
  : IDENTIFIER { getIdentifierLiteral $1 }

struct_decl :: { WithLocation (Definition Parsed) }
  : "struct" IDENTIFIER optional(generic_params) "{" comma_list(struct_field) "}" { WithLocation $1 (StructDef (StructInfo (getIdentifierLiteral $2) (fold $3) (NE.fromList $5))) }

struct_field :: { (Identifier, PathInfo Parsed) }
  : IDENTIFIER ":" type_expr { (getIdentifierLiteral $1, $3) }


const_decl :: { WithLocation (Definition Parsed) }
  : "const" IDENTIFIER ":" type_expr "=" expression ";" { WithLocation $1 (ConstDef (ConstInfo (getIdentifierLiteral $2) $4 $6)) }


fun_decl :: { WithLocation (Definition Parsed) }
  : "fn" IDENTIFIER optional(generic_params) "(" optional(comma_list(fun_arg)) ")" optional(fun_return) block { WithLocation $1 (FunctionDef (FunctionInfo (getIdentifierLiteral $2) (fold $3) (fold $5) $7 $8)) }

fun_arg :: { (Identifier, FunctionArgType Parsed) }
  : IDENTIFIER ":" fun_arg_type { (getIdentifierLiteral $1, $3) }

fun_arg_type :: { FunctionArgType Parsed }
  : type_expr { ByValue     $1 }
  | reference { ByReference $1 }

fun_return :: { PathInfo Parsed }
  : "->" type_expr { $2 }


generic_params :: { [Identifier] }
  : "<" comma_list(generic_param) ">" { $2 }

generic_param :: { Identifier }
  : IDENTIFIER { getIdentifierLiteral $1 }


block :: { [WithLocation (Statement Parsed)] }
  : "{" many(statement) "}" { $2 }

statement :: { WithLocation (Statement Parsed) }
  : block_stmt      { $1 }
  | inline_stmt ";" { $1 }

block_stmt :: { WithLocation (Statement Parsed) }
  : if_stmt    { WithLocation (fst $1) (IfStmt (snd $1)) }
  | for_stmt   { $1 }
  | while_stmt { $1 }

inline_stmt :: { WithLocation (Statement Parsed) }
  : let_stmt      { $1 }
  | return_stmt   { $1 }
  | continue_stmt { $1 }
  | break_stmt    { $1 }
  | expr_stmt     { $1 }


if_stmt :: { (Location, IfInfo Parsed) }
  : "if" expression block optional(else_stmt) { ($1, IfInfo $2 $3 $4) }

else_stmt  :: { ElseInfo Parsed }
  : "else" else_block { $2 }

else_block :: { ElseInfo Parsed }
 : if_stmt { ElseIf (snd $1) }
 | block   { ElseBlock $1 }

while_stmt :: { WithLocation (Statement Parsed) }
  : "while" expression block { WithLocation $1 (WhileStmt (WhileInfo $2 $3)) }

for_stmt :: { WithLocation (Statement Parsed) }
  : "for" IDENTIFIER "in" expression block { WithLocation $1 (ForStmt (ForInfo (getIdentifierLiteral $2) $4 $5)) }


let_stmt :: { WithLocation (Statement Parsed) }
  : "let" IDENTIFIER optional(let_type) "=" expression { WithLocation $1 (LetStmt (LetInfo (getIdentifierLiteral $2) $3 $5)) }

let_type :: { PathInfo Parsed }
  : ":" type_expr { $2 }

return_stmt :: { WithLocation (Statement Parsed) }
  : "return" optional(expression) { WithLocation $1 (ReturnStmt $2) }

continue_stmt :: { WithLocation (Statement Parsed) }
  : "continue" { WithLocation $1 ContinueStmt }

break_stmt :: { WithLocation (Statement Parsed) }
  : "break" { WithLocation $1 BreakStmt }

expr_stmt :: { WithLocation (Statement Parsed) }
  : expression { WithLocation (_location $1) (ExpressionStmt $1) }


expression :: { WithLocation (Expression Parsed) }
  : grouped_expr      { $1 }
  | path_expr         { WithLocation (fst $1) (PathExpr (snd $1)) }
  | field_access_expr { $1 }
  | call_expr         { $1 }
  | array_expr        { $1 }
  | index_expr        { $1 }
  | struct_expr       { $1 }
  | literal_expr      { $1 }
  | operator_expr     { $1 }

grouped_expr :: { WithLocation (Expression Parsed) }
  : "(" expression ")" { $2 }

path_expr :: { (Location, PathInfo Parsed) }
  : IDENTIFIER                   { (fst $1, PathInfo (pure $ getIdentifierLiteral $1) []) }
  | IDENTIFIER "::" generic_args { (fst $1, PathInfo (pure $ getIdentifierLiteral $1) $3) }
  | IDENTIFIER "::" path_expr    { (fst $1, prependPathInfo (getIdentifierLiteral $1) (snd $3)) }

field_access_expr :: { WithLocation (Expression Parsed) }
  : expression "." IDENTIFIER { WithLocation (_location $1) (FieldAccessExpr $1 (getIdentifierLiteral $3)) }

call_expr :: { WithLocation (Expression Parsed) }
  : path_expr "(" optional(comma_list(call_arg)) ")" { WithLocation (fst $1) (CallExpr (snd $1) (fold $3)) }

call_arg :: { WithLocation (Expression Parsed) }
  : expression { $1 }

array_expr :: { WithLocation (Expression Parsed) }
  : "[" optional(comma_list(array_element)) "]" { WithLocation $1 (ArrayExpr (fold $2)) }

array_element :: { WithLocation (Expression Parsed) }
  : expression { $1 }

index_expr :: { WithLocation (Expression Parsed) }
  : expression "[" expression "]" { WithLocation (_location $1) (IndexExpr $1 $3) }

struct_expr :: { WithLocation (Expression Parsed) }
  : path_expr "@" "{" comma_list(field_expr) "}" { WithLocation (fst $1) (StructExpr (snd $1) (NE.fromList $4)) }

field_expr :: { (Identifier, WithLocation (Expression Parsed)) }
  : IDENTIFIER ":" expression { (getIdentifierLiteral $1, $3) }

literal_expr :: { WithLocation (Expression Parsed) }
  : INT     { WithLocation (fst $1) (IntLiteralExpr    (getIntLiteral    $1)) }
  | CHAR    { WithLocation (fst $1) (CharLiteralExpr   (getCharLiteral   $1)) }
  | STRING  { WithLocation (fst $1) (StringLiteralExpr (getStringLiteral $1)) }
  | "true"  { WithLocation $1 (BoolLiteralExpr True)  }
  | "false" { WithLocation $1 (BoolLiteralExpr False) }

operator_expr :: { WithLocation (Expression Parsed) }
   : reference_expr           { $1 }
   | negation_expr            { $1 }
   | arithmetic_expr          { $1 }
   | comparison_expr          { $1 }
   | boolean_expr             { $1 }
   | cast_expr                { $1 }
   | range_expr               { $1 }
   | assignment_expr          { $1 }
   | compound_assignment_expr { $1 }

reference_expr :: { WithLocation (Expression Parsed) }
  : "&" path_expr %prec UNARY { WithLocation $1 (ReferenceExpr (snd $2)) }

negation_expr :: { WithLocation (Expression Parsed) }
  : "!" expression %prec UNARY { WithLocation $1 (BoolNegationExpr $2) }
  | "-" expression %prec UNARY { WithLocation $1 (IntNegationExpr  $2) }

arithmetic_expr :: { WithLocation (Expression Parsed) }
  : expression "+" expression { binaryExpr AdditionExpr       $1 $3 }
  | expression "-" expression { binaryExpr SubtractionExpr    $1 $3 }
  | expression "*" expression { binaryExpr MultiplicationExpr $1 $3 }
  | expression "/" expression { binaryExpr DivisionExpr       $1 $3 }
  | expression "%" expression { binaryExpr ModuloExpr         $1 $3 }
  | expression "^" expression { binaryExpr ExponentiationExpr $1 $3 }

comparison_expr :: { WithLocation (Expression Parsed) }
  : expression "==" expression { binaryExpr EqualityExpr   $1 $3 }
  | expression "!=" expression { binaryExpr DifferenceExpr $1 $3 }
  | expression ">"  expression { binaryExpr GreaterExpr    $1 $3 }
  | expression "<"  expression { binaryExpr LesserExpr     $1 $3 }
  | expression ">=" expression { binaryExpr GreaterEqExpr  $1 $3 }
  | expression "<=" expression { binaryExpr LesserEqExpr   $1 $3 }

boolean_expr :: { WithLocation (Expression Parsed) }
  : expression "&&" expression { binaryExpr BoolAndExpr $1 $3 }
  | expression "||" expression { binaryExpr BoolOrExpr  $1 $3 }

cast_expr :: { WithLocation (Expression Parsed) }
  : expression "as" path_expr { WithLocation (_location $1) (CastExpr $1 (snd $3)) }

range_expr :: { WithLocation (Expression Parsed) }
  : expression "..=" expression { binaryExpr RangeInclusiveExpr $1 $3 }
  | expression ".."  expression { binaryExpr RangeExclusiveExpr $1 $3 }

assignment_expr :: { WithLocation (Expression Parsed) }
  : expression "=" expression { binaryExpr AssignmentExpr $1 $3 }

compound_assignment_expr :: { WithLocation (Expression Parsed) }
  : expression "+=" expression { binaryExpr AdditionAssignmentExpr       $1 $3 }
  | expression "-=" expression { binaryExpr SubtractionAssignmentExpr    $1 $3 }
  | expression "*=" expression { binaryExpr MultiplicationAssignmentExpr $1 $3 }
  | expression "/=" expression { binaryExpr DivisionAssignmentExpr       $1 $3 }
  | expression "%=" expression { binaryExpr ModuloAssignmentExpr         $1 $3 }
  | expression "^=" expression { binaryExpr ExponentiationAssignmentExpr $1 $3 }


reference :: { PathInfo Parsed }
  : "&" type_expr { $2 }

type_expr :: { PathInfo Parsed }
  : IDENTIFIER                { PathInfo (pure $ getIdentifierLiteral $1) [] }
  | IDENTIFIER generic_args   { PathInfo (pure $ getIdentifierLiteral $1) $2 }
  | IDENTIFIER "::" type_expr { prependPathInfo (getIdentifierLiteral $1) $3 }

generic_args :: { [PathInfo Parsed] }
  : "<" comma_list(type_expr) ">" { $2 }


comma_list(p)
  : p many(comma_list_item(p)) optional(",") { $1 : $2 }

comma_list_item(p)
  : "," p { $2 }

optional(p)
  :   { Nothing }
  | p { Just $1 }

many(p)
  :           { []         }
  | many(p) p { $1 <> [$2] }


{

lexer :: ((Location, Token) -> Parser a) -> Parser a
lexer = (>>=) alexGetNextToken

getIdentifierLiteral :: (Location, Token) -> Identifier
getIdentifierLiteral (_, tok) = case tok of
  TIdentifier i -> i
  _             -> error "ICE: not an identifier"

getIntLiteral :: (Location, Token) -> Int
getIntLiteral (_, tok) = case tok of
  TLiteralInt i -> i
  _             -> error "ICE: not an int"

getCharLiteral :: (Location, Token) -> Char
getCharLiteral (_, tok) = case tok of
  TLiteralChar i -> i
  _              -> error "ICE: not a char"

getStringLiteral :: (Location, Token) -> Text
getStringLiteral (_, tok) = case tok of
  TLiteralString i -> i
  _                -> error "ICE: not a string"

prependPathInfo :: Identifier -> PathInfo Parsed -> PathInfo Parsed
prependPathInfo prepend = over pathName (prepend <|)

prependImport :: Identifier -> Import -> Import
prependImport prepend = over importPath (prepend <|)

binaryExpr
  :: (WithLocation (Expression Parsed) -> WithLocation (Expression Parsed) -> Expression Parsed)
  -> WithLocation (Expression Parsed)
  -> WithLocation (Expression Parsed)
  -> WithLocation (Expression Parsed)
binaryExpr cons exp1 exp2 = WithLocation (_location exp1) (cons exp1 exp2)

}
