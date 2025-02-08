module Lang.Pietre.Stages.Parsing where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Stages.Parsing.Monad
import Lang.Pietre.Stages.Parsing.Parser


parse :: FilePath -> Text -> Either ParseError File
parse = runParser parser
