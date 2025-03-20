{
module Lang.Pietre.Stages.Parsing.Parser where

import "this" Prelude

import Data.Text qualified as T
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad

}


%name moduleParser module
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
%left UNARY

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
  : use_path optional(use_alias)                 { Import $1 (Qualified $2) }
  | use_path "::" "*"                            { Import $1 Exhaustive }
  | use_path "::" "{" comma_list(IDENTIFIER) "}" { Import $1 (Specific (map getRawIdentifier $4)) }

use_alias :: { Identifier }
  : "as" IDENTIFIER { getRawIdentifier $2 }

use_path :: { [Identifier] }
  : IDENTIFIER many(use_path_item) { getRawIdentifier $1 : $2 }

use_path_item :: { Identifier }
  : "::" IDENTIFIER { getRawIdentifier $2 }


alias_decl :: { Declaration Parsed }
  : "type" IDENTIFIER optional(generic_params) "=" type_expr ";" { TypeAliasDecl $1 (TypeAliasInfo (getRawIdentifier $2) (fold $3) $5)}

enum_decl :: { Declaration Parsed }
  : "enum" IDENTIFIER "{" optional(comma_list(enum_item)) "}" { EnumDecl $1 (EnumInfo (getRawIdentifier $2) (fold $4)) }

enum_item :: { Identifier }
  : IDENTIFIER { getRawIdentifier $1 }

struct_decl :: { Declaration Parsed }
  : "struct" IDENTIFIER optional(generic_params) "{" comma_list(struct_field) "}" { StructDecl $1 (StructInfo (getRawIdentifier $2) (fold $3) $5) }

struct_field :: { (Identifier, TypeExpr Parsed) }
  : IDENTIFIER ":" type_expr { (getRawIdentifier $1, $3) }


const_decl :: { Declaration Parsed }
  : "const" IDENTIFIER ":" type_expr "=" expression ";" { ConstDecl $1 (ConstInfo (getRawIdentifier $2) $4 (snd $6)) }


fun_decl :: { Declaration Parsed }
  : "fn" IDENTIFIER optional(generic_params) "(" optional(comma_list(fun_arg)) ")" optional(fun_return) block { FunctionDecl $1 (FunctionInfo (getRawIdentifier $2) (fold $3) (fold $5) $7 $8) }

fun_arg :: { (Identifier, FunctionArgType Parsed) }
  : IDENTIFIER ":" fun_arg_type { (getRawIdentifier $1, $3) }

fun_arg_type :: { FunctionArgType Parsed }
  : type_expr { ByValue     $1 }
  | reference { ByReference $1 }

fun_return :: { TypeExpr Parsed }
  : "->" type_expr { $2 }


generic_params :: { [Identifier] }
  : "<" comma_list(generic_param) ">" { $2 }

generic_param :: { Identifier }
  : IDENTIFIER { getRawIdentifier $1 }


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
  : "for" IDENTIFIER "in" expression block { ForStmt $1 (ForInfo (getRawIdentifier $2) (snd $4) $5) }


let_stmt :: { Statement Parsed }
  : "let" IDENTIFIER optional(let_type) "=" expression { LetStmt $1 (LetInfo (getRawIdentifier $2) $3 (snd $5)) }

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
  : grouped_expr      { undefined }
  | path_expr         { undefined }
  | field_access_expr { undefined }
  | call_expr         { undefined }
  | array_expr        { undefined }
  | index_expr        { undefined }
  | struct_expr       { undefined }
  | literal_expr      { undefined }
  | operator_expr     { undefined }

grouped_expr :: { Expression Parsed }
  : "(" expression ")" { undefined }

path_expr :: { Expression Parsed }
  : IDENTIFIER many(path_expr_item) optional(generic_args) { undefined }

path_expr_item :: { Expression Parsed }
  : "::" IDENTIFIER { undefined }

field_access_expr :: { Expression Parsed }
  : expression "." IDENTIFIER { undefined }

call_expr :: { Expression Parsed }
  : path_expr "(" optional(comma_list(call_arg)) ")" { undefined }

call_arg :: { Expression Parsed }
  : expression { undefined }

array_expr :: { Expression Parsed }
  : "[" optional(comma_list(array_element)) "]" { undefined }

array_element :: { Expression Parsed }
  : expression { undefined }

index_expr :: { Expression Parsed }
  : expression "[" expression "]" { undefined }

struct_expr :: { Expression Parsed }
  : path_expr "{" comma_list(field_expr) "}" { undefined }

field_expr :: { Expression Parsed }
  : IDENTIFIER ":" expression { undefined }

literal_expr :: { Expression Parsed }
  : INT     { undefined }
  | CHAR    { undefined }
  | STRING  { undefined }
  | "true"  { undefined }
  | "false" { undefined }

operator_expr :: { Expression Parsed }
   : reference_expr           { undefined }
   | negation_expr            { undefined }
   | arithmetic_expr          { undefined }
   | comparison_expr          { undefined }
   | boolean_expr             { undefined }
   | cast_expr                { undefined }
   | range_expr               { undefined }
   | assignment_expr          { undefined }
   | compound_assignment_expr { undefined }

reference_expr :: { Expression Parsed }
  : "&" path_expr %prec UNARY { undefined }

negation_expr :: { Expression Parsed }
  : "!" expression %prec UNARY { undefined }
  | "-" expression %prec UNARY { undefined }

arithmetic_expr :: { Expression Parsed }
  : expression "+" expression { undefined }
  | expression "-" expression { undefined }
  | expression "*" expression { undefined }
  | expression "/" expression { undefined }
  | expression "%" expression { undefined }
  | expression "^" expression { undefined }

comparison_expr :: { Expression Parsed }
  : expression "==" expression { undefined }
  | expression "!=" expression { undefined }
  | expression ">"  expression { undefined }
  | expression "<"  expression { undefined }
  | expression ">=" expression { undefined }
  | expression "<=" expression { undefined }

boolean_expr :: { Expression Parsed }
  : expression "&&" expression { undefined }
  | expression "||" expression { undefined }

cast_expr :: { Expression Parsed }
  : expression "as" type_expr { undefined }

range_expr :: { Expression Parsed }
  : range_inclusive_expr { undefined }
  | range_exclusive_expr { undefined }

range_inclusive_expr :: { Expression Parsed }
  : expression "..=" expression { undefined }

range_exclusive_expr :: { Expression Parsed }
  : expression ".." expression { undefined }

assignment_expr :: { Expression Parsed }
  : expression "=" expression { undefined }

compound_assignment_expr :: { Expression Parsed }
  : expression "+=" expression { undefined }
  | expression "-=" expression { undefined }
  | expression "*=" expression { undefined }
  | expression "/=" expression { undefined }
  | expression "%=" expression { undefined }
  | expression "^=" expression { undefined }


reference :: { TypeExpr Parsed }
  : "&" type_expr { $2 }

type_expr :: { TypeExpr Parsed }
  : IDENTIFIER many(path_expr_item) optional(generic_args) { undefined }

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

getRawIdentifier :: (Location, Token) -> Identifier
getRawIdentifier (_, tok) = case tok of
  TIdentifier i -> i
  _             -> error "ICE: not an identifier"

}
