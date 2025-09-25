{- |

This module provides the API for the parsing phase: the raw source
file is converted into an AST, according to the rules of the grammar
(see `/docs/reference.md`).

-}


module Lang.Pietre.Stages.Parsing where

import "this" Prelude

import Control.Monad.Loops                  (unfoldM)
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad
import Lang.Pietre.Stages.Parsing.Parser


lex :: FilePath -> Text -> Either ParseError [(Location, Token)]
lex = runParser $ unfoldM $ fmap filterOutEOF alexGetNextToken
  where
    filterOutEOF = \case
      (_, TEOF) -> Nothing
      tokeninfo -> Just tokeninfo

parseModule :: FilePath -> Text -> Either ParseError Module
parseModule = runParser moduleParser

parseExpr :: FilePath -> Text -> Either ParseError (Expression Parsed)
parseExpr = fmap _located ... runParser expressionParser
