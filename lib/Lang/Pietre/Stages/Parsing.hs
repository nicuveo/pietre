module Lang.Pietre.Stages.Parsing where

import "this" Prelude

import Control.Monad.Loops                  (unfoldM)
import Data.Text                            qualified as T
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Lexer
import Lang.Pietre.Stages.Parsing.Monad
import Lang.Pietre.Stages.Parsing.Parser


lex :: FilePath -> Text -> Either ParseError [(Location, Token)]
lex = runParser (unfoldM go)
  where
    go = do
      currentState@ParserState{..} <- get
      case alexScan currentState 0 of
        AlexEOF                       -> pure Nothing
        AlexError newState            -> put newState >> alexError
        AlexSkip  newState _len       -> put newState >> go
        AlexToken newState len action -> do
          put newState
          let matchedText = T.take len _parserInput
          Just <$> action currentState _parserLocation matchedText

parse :: FilePath -> Text -> Either ParseError File
parse = runParser parseFile
