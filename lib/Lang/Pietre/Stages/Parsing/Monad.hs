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

-- | Implementation detail of Alex.
--
-- Alex is a code generator, and the generated haskell code makes several
-- assumptions, such as assuming that a function named `alexGetByte` exists
-- within the scope. The type of the state being passed is of our choosing, but
-- the overall shape isn't: given the previous state of the parser, this
-- function returns the next successfully parsed *byte* and the new parser
-- state, if we haven't reached the EOF yet.
--
-- Alex unfortunately works with bytes, and not unicode codepoints, so unicode
-- codepoints have to be decomposed into a series of individual bytes (see
-- 'decomposeUTF8').
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

-- | Implementation detail of Alex.
--
-- Alex assumes the existence of this function. Whenever a left context is
-- provided in a rule, Alex uses this function to get the previous lexed
-- character from our custom state.
alexInputPrevChar :: ParserState -> Char
alexInputPrevChar = view parserPrevChar

-- | Internal lexer function.
--
-- Aborts the current scan and report a 'ParseError'.
alexError :: Parser a
alexError = do
  Location filename _ line column <- use parserLocation
  throwError $ filename ++ ":" ++ show line ++ ":" ++ show column ++ ": lexical error"

-- | Scan any character.
--
-- This function attempts to lex the next character in the stream. If we're at
-- the end of the stream, an error is raised, otherwise the next character is
-- returned and the internal state is updated.
alexAny :: Parser Char
alexAny = do
  (match, remaining) <- uses parserInput T.uncons
    `onNothingM` alexError
  updateStateWith match remaining
  pure match

-- | Attempt to scan one character.
--
-- This function looks at the next input character, and successfully lexes it if
-- it matches the one given as input. Otherwise, nothing happens, and the
-- internal state is left unchanged. If there is no character left in the input
-- stream, an error is raised.
alexTry :: Char -> Parser Bool
alexTry expected = do
  (match, remaining) <- uses parserInput T.uncons
    `onNothingM` alexError
  let correct = match == expected
  when correct $ updateStateWith match remaining
  pure correct

-- | Attempt to scan a character that matches the given predicate.
--
-- Similar to 'alexTry', but takes a predicate rather than one given
-- character.
alexTryIf :: (Char -> Bool) -> Parser (Maybe Char)
alexTryIf predicate = do
  (match, remaining) <- uses parserInput T.uncons
    `onNothingM` alexError
  if predicate match
  then do
    updateStateWith match remaining
    pure $ Just match
  else
    pure Nothing

-- | Scan one given character.
--
-- Similar to 'alexTry', but raises an error if we fail to find the given
-- character.
alexExpect :: Char -> Parser ()
alexExpect expected =
  unlessM (alexTry expected) alexError

-- | Scan a character that matches the given predicate.
--
-- Similar to 'alexIfTry', but raises an error if we fail to find a matching
-- character.
alexExpectIf :: (Char -> Bool) -> Parser Char
alexExpectIf predicate =
  alexTryIf predicate `onNothingM` alexError

updateStateWith :: Char -> Text -> Parser ()
updateStateWith match remaining =
  modify \current@ParserState {..} ->
    current
      & parserLocation .~ updateLocation _parserLocation match
      & parserInput    .~ remaining
      & parserPrevChar .~ match

-- | Lex a character within a string literal.
--
-- This function is responsible for lexing one character within a string
-- literal, and handles escaping.
alexReadStringChar :: Parser Char
alexReadStringChar = do
  c1 <- alexAny
  if c1 /= '\\' then pure c1 else
    alexAny >>= \case
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

alexReadFirstIdentifierChar :: Parser Char
alexReadFirstIdentifierChar =
  alexExpectIf \c -> isAlpha c || c == '_'

alexReadIdentifierChar :: Parser (Maybe Char)
alexReadIdentifierChar =
  alexTryIf \c -> isAlphaNum c || c == '_'


-- happy functions

happyError :: ((Location, Token), [String]) -> Parser a
happyError ((Location filename _ line column, token), expected) = do
  throwError $ filename ++ ":" ++ show line ++ ":" ++ show column ++ ": parser error: " ++ show token ++ "; expecting:" ++ unwords expected
