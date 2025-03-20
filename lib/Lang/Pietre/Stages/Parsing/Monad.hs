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
import Control.Monad.Extra
import Data.Char
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
  { _parserInput    :: Text
  , _parserLocation :: Location
  , _parserPrevChar :: Char
  , _parserBytes    :: [Word8]
  } deriving Show

initialState :: FilePath -> Text -> ParserState
initialState filename source = ParserState
  { _parserInput     = source
  , _parserLocation  = initialLocation filename
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
  Location filename _ line column <- use parserLocation
  throwError $ filename ++ ":" ++ show line ++ ":" ++ show column ++ ": lexical error"

alexExpect :: Char -> Parser ()
alexExpect expected =
  unlessM (alexTry expected) alexError

alexTry :: Char -> Parser Bool
alexTry expected = do
  currentState <- get
  case alexGetByte currentState of
    Nothing -> pure False
    Just (byte, newState)
      | chr (fromIntegral byte) /= expected -> pure False
      | otherwise -> do
          put newState
          pure True

alexReadStringChar :: Parser Char
alexReadStringChar = do
  c1 <- getNextChar
  if c1 /= '\\' then pure c1 else
    getNextChar >>= \case
      '\n' -> undefined -- handleWhitespace
      'x'  -> undefined -- handleASCIIChar
      'u'  -> undefined -- handleUnicodeCodePoint
      'n'  -> pure '\n'
      'r'  -> pure '\r'
      't'  -> pure '\t'
      '0'  -> pure '\0'
      '\'' -> pure '\''
      '"'  -> pure '"'
      _    -> alexError
  where
    getNextChar =
      getMatchingChar (const True)
        `onNothingM` alexError

alexReadFirstIdentifierChar :: Parser Char
alexReadFirstIdentifierChar =
  getMatchingChar predicate
    `onNothingM` alexError
  where
    predicate c = isAlpha c || c == '_'

alexReadIdentifierChar :: Parser (Maybe Char)
alexReadIdentifierChar = getMatchingChar predicate
  where
    predicate c = isAlphaNum c || c == '_'

getMatchingChar :: (Char -> Bool) -> Parser (Maybe Char)
getMatchingChar predicate = do
  current@ParserState {..} <- get
  case T.uncons _parserInput of
    Nothing -> alexError
    Just (c, remaining)
      | predicate c -> do
          put $ current
            & parserLocation .~ updateLocation _parserLocation c
            & parserInput    .~ remaining
            & parserPrevChar .~ c
          pure (Just c)
      | otherwise -> pure Nothing


-- happy functions

happyError :: ((Location, Token), [String]) -> Parser a
happyError ((Location filename _ line column, token), expected) = do
  throwError $ filename ++ ":" ++ show line ++ ":" ++ show column ++ ": parser error: " ++ show token ++ "; expecting:" ++ unwords expected
