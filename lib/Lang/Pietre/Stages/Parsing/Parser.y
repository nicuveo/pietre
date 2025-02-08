{
module Lang.Pietre.Stages.Parsing.Parser where

import "this" Prelude

import Control.Lens ((.~))
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad
}


%name parser
%tokentype { Token }

%error { happyError }
%monad { Parser } { >>= } { return }
%lexer { lexer } { TEOF }


%token

const               { TKeyword KConst }

%%


file: statements { $1 }

statements
  : statements statement { $1 <> [$2] }
  | {- empty -}          { [] }

statement: keyword { $1 }

keyword: const const { () }


{

lexer :: (Token -> Parser a) -> Parser a
lexer f = do
  currentState <- get
  case alexScan currentState 0 of
    AlexEOF                       -> f TEOF
    AlexError newState            -> put newState >> alexError
    AlexSkip  newState _len       -> put newState >> (lexer f)
    AlexToken newState _len token -> put newState >> (f token)

}
