{
module Lang.Pietre.Stages.Parsing.Parser where

import "this" Prelude

import Control.Lens ((.~))
import Data.Text qualified as T
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad

}


%name parseFile file
%tokentype { (Location, Token) }

%error { happyError }
%monad { Parser } { >>= } { return }
%lexer { lexer } { (_, TEOF) }


%token

const               { (_, TKeywordConst) }

%%


file: statements { $1 }

statements
  : statements statement { $1 <> [$2] }
  | {- empty -}          { [] }

statement: keyword { $1 }

keyword: const const { () }


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
