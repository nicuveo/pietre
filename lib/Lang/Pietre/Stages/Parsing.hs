{- |

This module provides the API for the parsing phase: the raw source
file is converted into an AST, according to the rules of the grammar
(see `/docs/reference.md`).

-}


module Lang.Pietre.Stages.Parsing where

import "this" Prelude

import Control.Monad.Loops                    (unfoldM)
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad
import Lang.Pietre.Stages.Parsing.Parser


lex
  :: MonadDiagnosis m
  => FilePath
  -> Text
  -> m [(Location, Token)]
lex = runParser $ unfoldM $ fmap filterOutEOF alexGetNextToken
  where
    filterOutEOF = \case
      (_, TEOF) -> Nothing
      tokeninfo -> Just tokeninfo

parseModule
  :: MonadDiagnosis m
  => FilePath
  -> Text
  -> m Module
parseModule = runParser moduleParser

parseExpr
  :: MonadDiagnosis m
  => FilePath
  -> Text
  -> m Expression
parseExpr = fmap _located ... runParser expressionParser
