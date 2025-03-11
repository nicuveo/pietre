{- |

This module defines the underlying monad used by both Alex and Happy. It is
therefore used for both the lexing and parsing phases, which for the purpose of
this project are combined in one phase, simply referred to as "parsing". This is
because we use the "lexer" Happy option, which delegates to Happy the
responsibility of calling Alex: instead of first parsing the entire source as a
list of tokens, then applying the parser to the list of tokens, Happy calls Alex
as needed to make progress.

The core function that Alex needs is 'alexGetByte': given our current parsing
state, get the next byte from the input and the new state. Additionally, we
define custom error handling functions.

Only one required function isn't defined in this file: the bridge between Happy
and Alex is done directly in Parser.y, as it requires knowledge of some Alex
internals that are not accessible from this module.

-}

{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Parsing.Monad where

import "this" Prelude

import Control.Lens
import Data.Text                            qualified as T
import Data.Word                            (Word8)
import Lang.Pietre.Internal.Encoding
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens


-- Parser monad

newtype Parser a = Parser (ParserState -> Either ParseError (a, ParserState))
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState ParserState
    , MonadError ParseError
    ) via (StateT ParserState (Except ParseError))

runParser :: Parser a -> FilePath -> Text -> Either ParseError a
runParser (Parser f) filename source = fmap fst $ f $ initialState filename source


-- internal state

data ParserState = ParserState
  { _parserFileName :: FilePath
  , _parserInput    :: Text
  , _parserLocation :: Location
  , _parserPrevChar :: Char
  , _parserBytes    :: [Word8]
  } deriving Show

initialState :: FilePath -> Text -> ParserState
initialState filename source = ParserState
  { _parserFileName  = filename
  , _parserInput     = source
  , _parserLocation  = initialLocation
  , _parserPrevChar  = '\n'
  , _parserBytes     = []
  }


-- error

type ParseError = String


-- lens generation

makeLenses ''ParserState


-- alex functions

type AlexInput = ParserState

alexGetByte :: ParserState -> Maybe (Word8, ParserState)
alexGetByte prev@ParserState {..} = case _parserBytes of
  (b:bs) -> Just (b, prev & parserBytes .~ bs)
  []     -> do
    (c, remaining) <- T.uncons _parserInput
    let b :| bytes = decomposeUTF8 c
        newPos     = updateLocation _parserLocation c
        newState   = prev
          & parserLocation .~ newPos
          & parserInput    .~ remaining
          & parserPrevChar .~ c
          & parserBytes    .~ bytes
    Just (b, newState)

alexInputPrevChar :: ParserState -> Char
alexInputPrevChar = view parserPrevChar

alexError :: Parser a
alexError = do
  Location _ line column <- use parserLocation
  throwError $ "lexical error at line " ++ show line ++ ", column " ++ show column


-- happy functions

happyError :: Token -> Parser a
happyError _ = do
  Location _ line column <- use parserLocation
  throwError $ "parse error at line " ++ show line ++ ", column " ++ show column
