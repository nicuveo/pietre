{
module Lang.Pietre.Stages.Parsing.Parser where

import "this" Prelude

import Data.Text qualified as T
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad

}


%name parseModule module
%tokentype { (Location, Token) }

%error { happyError }
%monad { Parser } { >>= } { return }
%lexer { lexer } { (_, TEOF) }

%nonassoc "..=" ".."
%left "||"
%left "&&"
%nonassoc "<=" "<" ">=" ">" "==" "!="
%left "+" "-"
%left "*" "/" "%"
%left "^"
%left UNARY

%token

as       { (_, TKeywordAs)              }
break    { (_, TKeywordBreak)           }
const    { (_, TKeywordConst)           }
continue { (_, TKeywordContinue)        }
else     { (_, TKeywordElse)            }
enum     { (_, TKeywordEnum)            }
false    { (_, TKeywordFalse)           }
fn       { (_, TKeywordFn)              }
for      { (_, TKeywordFor)             }
if       { (_, TKeywordIf)              }
in       { (_, TKeywordIn)              }
let      { (_, TKeywordLet)             }
return   { (_, TKeywordReturn)          }
struct   { (_, TKeywordStruct)          }
true     { (_, TKeywordTrue)            }
type     { (_, TKeywordType)            }
use      { (_, TKeywordUse)             }
while    { (_, TKeywordWhile)           }
";"      { (_, TOperatorSemicolon)      }
"::"     { (_, TOperatorType)           }
"*"      { (_, TOperatorStar)           }
","      { (_, TOperatorComma)          }
"="      { (_, TOperatorAssign)         }
":"      { (_, TOperatorColon)          }
"->"     { (_, TOperatorArrow)          }
"<"      { (_, TOperatorLessThan)       }
">"      { (_, TOperatorGreaterThan)    }
"."      { (_, TOperatorDot)            }
"..="    { (_, TOperatorRangeInclusive) }
".."     { (_, TOperatorRangeExclusive) }
"&"      { (_, TOperatorReference)      }
"!"      { (_, TOperatorNot)            }
"-"      { (_, TOperatorMinus)          }
"+"      { (_, TOperatorPlus)           }
"/"      { (_, TOperatorDiv)            }
"%"      { (_, TOperatorMod)            }
"^"      { (_, TOperatorPow)            }
"=="     { (_, TOperatorEqual)          }
"!="     { (_, TOperatorDiff)           }
">="     { (_, TOperatorGreaterOrEqual) }
"<="     { (_, TOperatorLessOrEqual)    }
"&&"     { (_, TOperatorBoolAnd)        }
"||"     { (_, TOperatorBoolOr)         }
"+="     { (_, TOperatorAssignPlus)     }
"-="     { (_, TOperatorAssignMinus)    }
"*="     { (_, TOperatorAssignMult)     }
"/="     { (_, TOperatorAssignDiv)      }
"%="     { (_, TOperatorAssignMod)      }
"^="     { (_, TOperatorAssignPow)      }
"("      { (_, TDelimiterParensOpen)    }
")"      { (_, TDelimiterParensClose)   }
"{"      { (_, TDelimiterBracesOpen)    }
"}"      { (_, TDelimiterBracesClose)   }
"["      { (_, TDelimiterBracketsOpen)  }
"]"      { (_, TDelimiterBracketsClose) }

int_literal    { (_, TLiteralInt    _) }
char_literal   { (_, TLiteralChar   _) }
string_literal { (_, TLiteralString _) }

identifier { (_, TIdentifier $$) }

%%

module :: { Module }
  : declarations { mkModule $1 }

declarations :: { [Either Import Declaration] }
  : many(declaration) { $1 }

declaration :: { Either Import Declaration }
  : use_decl   { Left  $1 }
  | type_decl  { Right $1 }
  | const_decl { Right $1 }
  | fun_decl   { Right $1 }


use_decl :: { Import }
  : use use_tree ";" { $2 }

use_tree :: { Import }
  : use_path optional(use_alias)                 { undefined }
  | use_path "::" "*"                            { undefined }
  | use_path "::" "{" comma_list(identifier) "}" { undefined }

use_alias :: { Identifier }
  : as identifier { $2 }

use_path :: { [Identifier] }
  : identifier many(use_path_item) { $1 : $2 }

use_path_item :: { Identifier }
  : "::" identifier { $2 }


type_decl :: { Declaration }
  : alias_decl  { undefined }
  | enum_decl   { undefined }
  | struct_decl { undefined }

alias_decl :: { _ }
  : type identifier optional(generic_params) "=" type_expr ";" { undefined }

enum_decl :: { _ }
  : enum identifier "{" optional(comma_list(enum_item)) "}" { undefined }

enum_item :: { Identifier }
  : identifier { undefined }

struct_decl :: { _ }
  : struct identifier optional(generic_params) "{" comma_list(struct_field) "}" { undefined }

struct_field :: { _ }
  : identifier ":" type_expr { ($1, $3) }


const_decl :: { _ }
  : const identifier ":" type_expr "=" expression ";" { undefined }


fun_decl :: { _ }
  : fn identifier optional(generic_params) "(" optional(comma_list(fun_arg)) ")" optional(fun_return) block { undefined }

fun_arg :: { _ }
  : identifier ":" fun_arg_type { undefined }

fun_arg_type :: { _ }
  : type_expr      { undefined}
  | reference { undefined }

fun_return :: { _ }
  : "->" type_expr { undefined }


generic_params :: { [_] }
  : "<" comma_list(generic_param) ">" { undefined }

generic_param :: { _ }
  : identifier { undefined }


block :: { [Statement] }
  : "{" many(statement) "}" { $2 }

statement :: { Statement }
  : block_stmt      { $1 }
  | inline_stmt ";" { $1 }

block_stmt :: { Statement }
  : if_stmt    { undefined }
  | for_stmt   { undefined }
  | while_stmt { undefined }

inline_stmt :: { Statement }
  : let_stmt      { undefined }
  | return_stmt   { undefined }
  | continue_stmt { undefined }
  | break_stmt    { undefined }
  | expression    { undefined }


if_stmt :: { Statement }
  : if expression block optional(else_stmt) { undefined }

else_stmt  :: { _ }
  : else else_block { undefined }

else_block :: { _ }
 : if_stmt { undefined }
 | block   { undefined }

while_stmt :: { _ }
  : while expression block { undefined }

for_stmt :: { _ }
  : for identifier in expression block { undefined }


let_stmt :: { _ }
  : let identifier optional(let_type) "=" expression { undefined }

let_type :: { _ }
  : ":" type_expr { undefined }

return_stmt :: { _ }
  : return optional(expression) { undefined }

continue_stmt :: { _ }
  : continue { undefined }

break_stmt :: { _ }
  : break { undefined }

expression :: { Expression }
  : grouped_expr      { undefined }
  | path_expr         { undefined }
  | field_access_expr { undefined }
  | call_expr         { undefined }
  | array_expr        { undefined }
  | index_expr        { undefined }
  | struct_expr       { undefined }
  | literal_expr      { undefined }
  | operator_expr     { undefined }

grouped_expr :: { Expression }
  : "(" expression ")" { undefined }

path_expr :: { Expression }
  : identifier many(path_expr_item) optional(generic_args) { undefined }

path_expr_item :: { Expression }
  : "::" identifier { $2 }

field_access_expr :: { Expression }
  : expression "." identifier { undefined }

call_expr :: { Expression }
  : path_expr "(" optional(comma_list(call_arg)) ")" { undefined }

call_arg :: { Expression }
  : expression { undefined }

array_expr :: { Expression }
  : "[" optional(comma_list(array_element)) "]" { undefined }

array_element :: { Expression }
  : expression { undefined }

index_expr :: { Expression }
  : expression "[" expression "]" { undefined }

struct_expr :: { Expression }
  : path_expr "{" comma_list(field_expr) "}" { undefined }

field_expr :: { Expression }
  : identifier ":" expression { undefined }

literal_expr :: { Expression }
  : int_literal    { undefined }
  | char_literal   { undefined }
  | string_literal { undefined }
  | true           { undefined }
  | false          { undefined }

operator_expr :: { Expression }
   : reference_expr %prec UNARY { undefined }
   | negation_expr  %prec UNARY { undefined }
   | arithmetic_expr          { undefined }
   | comparison_expr          { undefined }
   | boolean_expr             { undefined }
   | cast_expr                { undefined }
   | range_expr               { undefined }
   | assignment_expr          { undefined }
   | compound_assignment_expr { undefined }

reference_expr :: { Expression }
  : "&" path_expr { undefined }

negation_expr :: { Expression }
  : "!" expression { undefined }
  | "-" expression { undefined }

arithmetic_expr :: { Expression }
  : expression "+" expression { undefined }
  | expression "-" expression { undefined }
  | expression "*" expression { undefined }
  | expression "/" expression { undefined }
  | expression "%" expression { undefined }
  | expression "^" expression { undefined }

comparison_expr :: { Expression }
  : expression "==" expression { undefined }
  | expression "!=" expression { undefined }
  | expression ">"  expression { undefined }
  | expression "<"  expression { undefined }
  | expression ">=" expression { undefined }
  | expression "<=" expression { undefined }

boolean_expr :: { Expression }
  : expression "&&" expression { undefined }
  | expression "||" expression { undefined }

cast_expr :: { Expression }
  : expression as type { undefined }

range_expr :: { Expression }
  : range_inclusive_expr { undefined }
  | range_exclusive_expr { undefined }

range_inclusive_expr :: { Expression }
  : expression "..=" expression { undefined }

range_exclusive_expr :: { Expression }
  : expression ".." expression { undefined }

assignment_expr :: { Expression }
  : expression "=" expression { undefined }

compound_assignment_expr :: { Expression }
  : expression "+=" expression { undefined }
  | expression "-=" expression { undefined }
  | expression "*=" expression { undefined }
  | expression "/=" expression { undefined }
  | expression "%=" expression { undefined }
  | expression "^=" expression { undefined }


reference :: { _ }
  : "&" type_expr { undefined }

type_expr :: { _ }
  : identifier many(path_expr_item) optional(generic_args) { undefined }

generic_args :: { _ }
  : "<" comma_list(type) ">" { undefined }


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
lexer f = do
  currentState@ParserState {..} <- get
  case alexScan currentState 0 of
    AlexEOF ->
      f (_parserLocation, TEOF)
    AlexError newState -> do
      put newState
      alexError
    AlexSkip  newState _len -> do
      put newState
      lexer f
    AlexToken newState len action -> do
      put newState
      let matchedText = T.take len _parserInput
      action currentState _parserLocation matchedText >>= f

}
