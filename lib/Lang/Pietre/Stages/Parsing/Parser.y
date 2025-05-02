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

module :: { Module Parsed }
  : many(declaration) { mconcat $1 }

declaration :: { Module Parsed }
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


alias_decl :: { Declaration Parsed }
  : "type" IDENTIFIER optional(generic_params) "=" type_expr ";" { TypeAliasDecl $1 (TypeAliasInfo (getIdentifierLiteral $2) (fold $3) $5)}

enum_decl :: { Declaration Parsed }
  : "enum" IDENTIFIER "{" optional(comma_list(enum_item)) "}" { EnumDecl $1 (EnumInfo (getIdentifierLiteral $2) (fold $4)) }

enum_item :: { Identifier }
  : IDENTIFIER { getIdentifierLiteral $1 }

struct_decl :: { Declaration Parsed }
  : "struct" IDENTIFIER optional(generic_params) "{" comma_list(struct_field) "}" { StructDecl $1 (StructInfo (getIdentifierLiteral $2) (fold $3) (NE.fromList $5)) }

struct_field :: { (Identifier, TypeExpr Parsed) }
  : IDENTIFIER ":" type_expr { (getIdentifierLiteral $1, $3) }


const_decl :: { Declaration Parsed }
  : "const" IDENTIFIER ":" type_expr "=" expression ";" { ConstDecl $1 (ConstInfo (getIdentifierLiteral $2) $4 (snd $6)) }


fun_decl :: { Declaration Parsed }
  : "fn" IDENTIFIER optional(generic_params) "(" optional(comma_list(fun_arg)) ")" optional(fun_return) block { FunctionDecl $1 (FunctionInfo (getIdentifierLiteral $2) (fold $3) (fold $5) $7 $8) }

fun_arg :: { (Identifier, FunctionArgType Parsed) }
  : IDENTIFIER ":" fun_arg_type { (getIdentifierLiteral $1, $3) }

fun_arg_type :: { FunctionArgType Parsed }
  : type_expr { ByValue     $1 }
  | reference { ByReference $1 }

fun_return :: { TypeExpr Parsed }
  : "->" type_expr { $2 }


generic_params :: { [Identifier] }
  : "<" comma_list(generic_param) ">" { $2 }

generic_param :: { Identifier }
  : IDENTIFIER { getIdentifierLiteral $1 }


block :: { [Statement Parsed] }
  : "{" many(statement) "}" { $2 }

statement :: { Statement Parsed }
  : block_stmt      { $1 }
  | inline_stmt ";" { $1 }

block_stmt :: { Statement Parsed }
  : if_stmt    { uncurry IfStmt $1 }
  | for_stmt   { $1 }
  | while_stmt { $1 }

inline_stmt :: { Statement Parsed }
  : let_stmt      { $1 }
  | return_stmt   { $1 }
  | continue_stmt { $1 }
  | break_stmt    { $1 }
  | expr_stmt     { $1 }


if_stmt :: { (Location, IfInfo Parsed) }
  : "if" expression block optional(else_stmt) { ($1, IfInfo (snd $2) $3 $4) }

else_stmt  :: { ElseInfo Parsed }
  : "else" else_block { $2 }

else_block :: { ElseInfo Parsed }
 : if_stmt { ElseIf (snd $1) }
 | block   { ElseBlock $1    }

while_stmt :: { Statement Parsed }
  : "while" expression block { WhileStmt $1 (WhileInfo (snd $2) $3) }

for_stmt :: { Statement Parsed }
  : "for" IDENTIFIER "in" expression block { ForStmt $1 (ForInfo (getIdentifierLiteral $2) (snd $4) $5) }


let_stmt :: { Statement Parsed }
  : "let" IDENTIFIER optional(let_type) "=" expression { LetStmt $1 (LetInfo (getIdentifierLiteral $2) $3 (snd $5)) }

let_type :: { TypeExpr Parsed }
  : ":" type_expr { $2 }

return_stmt :: { Statement Parsed }
  : "return" optional(expression) { ReturnStmt $1 (fmap snd $2) }

continue_stmt :: { Statement Parsed }
  : "continue" { ContinueStmt $1 }

break_stmt :: { Statement Parsed }
  : "break" { BreakStmt $1 }

expr_stmt :: { Statement Parsed }
  : expression { uncurry ExpressionStmt $1 }


expression :: { (Location, Expression Parsed) }
  : grouped_expr      { $1 }
  | path_expr         { (fst $1, PathExpr (fst $1) (snd $1)) }
  | field_access_expr { $1 }
  | call_expr         { $1 }
  | array_expr        { $1 }
  | index_expr        { $1 }
  | struct_expr       { $1 }
  | literal_expr      { $1 }
  | operator_expr     { $1 }

grouped_expr :: { (Location, Expression Parsed) }
  : "(" expression ")" { $2 }

path_expr :: { (Location, PathInfo Parsed) }
  : IDENTIFIER                   { (fst $1, PathInfo (pure $ getIdentifierLiteral $1) []) }
  | IDENTIFIER "::" generic_args { (fst $1, PathInfo (pure $ getIdentifierLiteral $1) $3) }
  | IDENTIFIER "::" path_expr    { (fst $1, prependPathInfo (getIdentifierLiteral $1) (snd $3)) }

field_access_expr :: { (Location, Expression Parsed) }
  : expression "." IDENTIFIER { let (l, e) = $1 in (l, FieldAccessExpr l e (getIdentifierLiteral $3)) }

call_expr :: { (Location, Expression Parsed) }
  : path_expr "(" optional(comma_list(call_arg)) ")" { let (l, pi) = $1 in (l, CallExpr l pi (fold $3)) }

call_arg :: { Expression Parsed }
  : expression { snd $1 }

array_expr :: { (Location, Expression Parsed) }
  : "[" optional(comma_list(array_element)) "]" { ($1, ArrayExpr $1 (fold $2)) }

array_element :: { Expression Parsed }
  : expression { snd $1 }

index_expr :: { (Location, Expression Parsed) }
  : expression "[" expression "]" { (fst $1, IndexExpr (fst $1) (snd $1) (snd $3)) }

struct_expr :: { (Location, Expression Parsed) }
  : path_expr "@" "{" comma_list(field_expr) "}" { (fst $1, StructExpr (fst $1) (snd $1) $3) }

field_expr :: { (Identifier, Expression Parsed) }
  : IDENTIFIER ":" expression { (getIdentifierLiteral $1, snd $3) }

literal_expr :: { (Location, Expression Parsed) }
  : INT     { (fst $1, IntLiteralExpr    (fst $1) (getIntLiteral    $1)) }
  | CHAR    { (fst $1, CharLiteralExpr   (fst $1) (getCharLiteral   $1)) }
  | STRING  { (fst $1, StringLiteralExpr (fst $1) (getStringLiteral $1)) }
  | "true"  { ($1, BoolLiteralExpr   $1 True)  }
  | "false" { ($1, BoolLiteralExpr   $1 False) }

operator_expr :: { (Location, Expression Parsed) }
   : reference_expr           { $1 }
   | negation_expr            { $1 }
   | arithmetic_expr          { $1 }
   | comparison_expr          { $1 }
   | boolean_expr             { $1 }
   | cast_expr                { $1 }
   | range_expr               { $1 }
   | assignment_expr          { $1 }
   | compound_assignment_expr { $1 }

reference_expr :: { (Location, Expression Parsed) }
  : "&" path_expr %prec UNARY { ($1, ReferenceExpr $1 (snd $2)) }

negation_expr :: { (Location, Expression Parsed) }
  : "!" expression %prec UNARY { ($1, NegationExpr $1 (snd $2)) }
  | "-" expression %prec UNARY { ($1, NegationExpr $1 (snd $2)) }

arithmetic_expr :: { (Location, Expression Parsed) }
  : expression "+" expression { binaryExpr AdditionExpr       $1 $3 }
  | expression "-" expression { binaryExpr SubtractionExpr    $1 $3 }
  | expression "*" expression { binaryExpr MultiplicationExpr $1 $3 }
  | expression "/" expression { binaryExpr DivisionExpr       $1 $3 }
  | expression "%" expression { binaryExpr ModuloExpr         $1 $3 }
  | expression "^" expression { binaryExpr ExponentiationExpr $1 $3 }

comparison_expr :: { (Location, Expression Parsed) }
  : expression "==" expression { binaryExpr EqualityExpr   $1 $3 }
  | expression "!=" expression { binaryExpr DifferenceExpr $1 $3 }
  | expression ">"  expression { binaryExpr GreaterExpr    $1 $3 }
  | expression "<"  expression { binaryExpr LesserExpr     $1 $3 }
  | expression ">=" expression { binaryExpr GreaterEqExpr  $1 $3 }
  | expression "<=" expression { binaryExpr LesserEqExpr   $1 $3 }

boolean_expr :: { (Location, Expression Parsed) }
  : expression "&&" expression { binaryExpr BoolAndExpr $1 $3 }
  | expression "||" expression { binaryExpr BoolOrExpr  $1 $3 }

cast_expr :: { (Location, Expression Parsed) }
  : expression "as" type_expr { (fst $1, CastExpr (fst $1) (snd $1) $3) }

range_expr :: { (Location, Expression Parsed) }
  : expression "..=" expression { binaryExpr RangeInclusiveExpr $1 $3 }
  | expression ".."  expression { binaryExpr RangeExclusiveExpr $1 $3 }

assignment_expr :: { (Location, Expression Parsed) }
  : expression "=" expression { binaryExpr AssignmentExpr $1 $3 }

compound_assignment_expr :: { (Location, Expression Parsed) }
  : expression "+=" expression { binaryExpr AdditionAssignmentExpr       $1 $3 }
  | expression "-=" expression { binaryExpr SubtractionAssignmentExpr    $1 $3 }
  | expression "*=" expression { binaryExpr MultiplicationAssignmentExpr $1 $3 }
  | expression "/=" expression { binaryExpr DivisionAssignmentExpr       $1 $3 }
  | expression "%=" expression { binaryExpr ModuloAssignmentExpr         $1 $3 }
  | expression "^=" expression { binaryExpr ExponentiationAssignmentExpr $1 $3 }


reference :: { TypeExpr Parsed }
  : "&" type_expr { $2 }

type_expr :: { TypeExpr Parsed }
  : IDENTIFIER                   { PathInfo (pure $ getIdentifierLiteral $1) [] }
  | IDENTIFIER "::" generic_args { PathInfo (pure $ getIdentifierLiteral $1) $3 }
  | IDENTIFIER "::" type_expr    { prependPathInfo (getIdentifierLiteral $1) $3 }

generic_args :: { [TypeExpr Parsed] }
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
  :: (Location -> Expression Parsed -> Expression Parsed -> Expression Parsed)
  -> (Location, Expression Parsed)
  -> (Location, Expression Parsed)
  -> (Location, Expression Parsed)
binaryExpr cons (location, exp1) (_, exp2) = (location, cons location exp1 exp2)

}
