{

module Lang.Pietre.Stages.Parsing.Lexer where

import "this" Prelude

import Lang.Pietre.Stages.Parsing.Monad
import Lang.Pietre.Representations.Tokens

}


tokens :-
  $white+           ;
  "const"           { TKeyword KConst }
