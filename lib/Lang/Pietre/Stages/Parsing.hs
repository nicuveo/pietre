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

parse :: FilePath -> Text -> Either ParseError File
parse = runParser parseFile
