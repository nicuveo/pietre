{

module Lang.Pietre.Stages.Parsing.Lexer where

import "this" Prelude

import Control.Monad.Extra (whenM)
import Control.Monad.Loops (whileM, unfoldM)
import Data.Char (digitToInt)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Lang.Pietre.Representations.Location hiding (location)
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Monad

}

$symbol = [!\"\%&'\(\)\*\+\,\-\.\/:\;\<\>\=\[\]\^\{\|\}]
@keyword_border = $white | $symbol

tokens :-
  $white+                      ;

  "as"       / @keyword_border { mkToken TKeywordAs              }
  "break"    / @keyword_border { mkToken TKeywordBreak           }
  "const"    / @keyword_border { mkToken TKeywordConst           }
  "continue" / @keyword_border { mkToken TKeywordContinue        }
  "else"     / @keyword_border { mkToken TKeywordElse            }
  "enum"     / @keyword_border { mkToken TKeywordEnum            }
  "false"    / @keyword_border { mkToken TKeywordFalse           }
  "fn"       / @keyword_border { mkToken TKeywordFn              }
  "for"      / @keyword_border { mkToken TKeywordFor             }
  "if"       / @keyword_border { mkToken TKeywordIf              }
  "in"       / @keyword_border { mkToken TKeywordIn              }
  "let"      / @keyword_border { mkToken TKeywordLet             }
  "return"   / @keyword_border { mkToken TKeywordReturn          }
  "struct"   / @keyword_border { mkToken TKeywordStruct          }
  "true"     / @keyword_border { mkToken TKeywordTrue            }
  "type"     / @keyword_border { mkToken TKeywordType            }
  "use"      / @keyword_border { mkToken TKeywordUse             }
  "while"    / @keyword_border { mkToken TKeywordWhile           }
  ";"                          { mkToken TOperatorSemicolon      }
  "::"                         { mkToken TOperatorType           }
  "*"                          { mkToken TOperatorStar           }
  ","                          { mkToken TOperatorComma          }
  "="                          { mkToken TOperatorAssign         }
  ":"                          { mkToken TOperatorColon          }
  "->"                         { mkToken TOperatorArrow          }
  "<"                          { mkToken TOperatorLessThan       }
  ">"                          { mkToken TOperatorGreaterThan    }
  "."                          { mkToken TOperatorDot            }
  "..="                        { mkToken TOperatorRangeInclusive }
  ".."                         { mkToken TOperatorRangeExclusive }
  "&"                          { mkToken TOperatorReference      }
  "!"                          { mkToken TOperatorNot            }
  "-"                          { mkToken TOperatorMinus          }
  "+"                          { mkToken TOperatorPlus           }
  "/"                          { mkToken TOperatorDiv            }
  "%"                          { mkToken TOperatorMod            }
  "^"                          { mkToken TOperatorPow            }
  "=="                         { mkToken TOperatorEqual          }
  "!="                         { mkToken TOperatorDiff           }
  ">="                         { mkToken TOperatorGreaterOrEqual }
  "<="                         { mkToken TOperatorLessOrEqual    }
  "&&"                         { mkToken TOperatorBoolAnd        }
  "||"                         { mkToken TOperatorBoolOr         }
  "+="                         { mkToken TOperatorAssignPlus     }
  "-="                         { mkToken TOperatorAssignMinus    }
  "*="                         { mkToken TOperatorAssignMult     }
  "/="                         { mkToken TOperatorAssignDiv      }
  "%="                         { mkToken TOperatorAssignMod      }
  "^="                         { mkToken TOperatorAssignPow      }
  "("                          { mkToken TDelimiterParensOpen    }
  ")"                          { mkToken TDelimiterParensClose   }
  "{"                          { mkToken TDelimiterBracesOpen    }
  "}"                          { mkToken TDelimiterBracesClose   }
  "["                          { mkToken TDelimiterBracketsOpen  }
  "]"                          { mkToken TDelimiterBracketsClose }

  "//"[^\n]*                   ;
  "/*"                         { mkComment }

  [0-9]+                       { mkDecimalLiteral                }
  "0x"[0-9A-Fa-f]+             { mkHexadecimalLiteral            }
  "'"                          { mkCharLiteral                   }
  \"                           { mkStringLiteral                 }

  .                            { mkIdentifier                    }


{

type AlexAction = ParserState -> Location -> Text -> Parser (Location, Token)

mkToken :: Token -> AlexAction
mkToken tok _ location _ = pure (location, tok)

mkDecimalLiteral :: AlexAction
mkDecimalLiteral _ location matched = case T.decimal matched of
  Right (intValue, remaining)
    | T.null remaining -> pure (location, TLiteralInt intValue)
  _ -> alexError

mkHexadecimalLiteral :: AlexAction
mkHexadecimalLiteral _ location matched = case T.hexadecimal matched of
  Right (intValue, remaining)
    | T.null remaining -> pure (location, TLiteralInt intValue)
  _ -> alexError

mkCharLiteral :: AlexAction
mkCharLiteral _ location _ = do
  whenM (alexTry '\'')
    alexError
  c <- alexReadStringChar
  alexExpect '\''
  pure (location, TLiteralChar c)

mkStringLiteral :: AlexAction
mkStringLiteral _ location _ = do
  s <- readStringChars
  pure (location, TLiteralString $ T.pack s)
  where
    readStringChars =
      whileM (not <$> alexTry '"')
        alexReadStringChar

mkIdentifier :: AlexAction
mkIdentifier prevState location _ = do
  put prevState
  firstCharacter <- alexReadFirstIdentifierChar
  remainingCharacters <- unfoldM alexReadIdentifierChar
  pure (location, TIdentifier $ T.pack (firstCharacter : remainingCharacters))

mkComment :: AlexAction
mkComment _ _ _ = do
  whileM (not <$> endOfComment) $
    void $ getMatchingChar (const True)
  alexGetNextToken
  where
    endOfComment = do
      currentState <- get
      atTheEnd <- liftA2 (&&) (alexTry '*') (alexTry '/')
      unless atTheEnd $ put currentState
      pure atTheEnd

alexGetNextToken :: Parser (Location, Token)
alexGetNextToken = do
  currentState@ParserState {..} <- get
  case alexScan currentState 0 of
    AlexEOF ->
      pure (_parserLocation, TEOF)
    AlexError newState -> do
      put newState
      alexError
    AlexSkip  newState _len -> do
      put newState
      alexGetNextToken
    AlexToken newState len action -> do
      put newState
      let matchedText = T.take len _parserInput
      action currentState _parserLocation matchedText

}
