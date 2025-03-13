{

module Lang.Pietre.Stages.Parsing.Lexer where

import "this" Prelude

import Control.Monad.Extra (whenM)
import Control.Monad.Loops (whileM, unfoldM)
import Data.Char (digitToInt)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Parsing.Monad

}


tokens :-
  $white+    ;

  "as"       { mkToken TKeywordAs              }
  "break"    { mkToken TKeywordBreak           }
  "const"    { mkToken TKeywordConst           }
  "continue" { mkToken TKeywordContinue        }
  "else"     { mkToken TKeywordElse            }
  "enum"     { mkToken TKeywordEnum            }
  "false"    { mkToken TKeywordFalse           }
  "fn"       { mkToken TKeywordFn              }
  "for"      { mkToken TKeywordFor             }
  "if"       { mkToken TKeywordIf              }
  "in"       { mkToken TKeywordIn              }
  "let"      { mkToken TKeywordLet             }
  "return"   { mkToken TKeywordReturn          }
  "struct"   { mkToken TKeywordStruct          }
  "true"     { mkToken TKeywordTrue            }
  "type"     { mkToken TKeywordType            }
  "use"      { mkToken TKeywordUse             }
  "while"    { mkToken TKeywordWhile           }
  ";"        { mkToken TOperatorSemicolon      }
  "::"       { mkToken TOperatorType           }
  "*"        { mkToken TOperatorStar           }
  ","        { mkToken TOperatorComma          }
  "="        { mkToken TOperatorAssign         }
  ":"        { mkToken TOperatorColon          }
  "->"       { mkToken TOperatorArrow          }
  "<"        { mkToken TOperatorLessThan       }
  ">"        { mkToken TOperatorGreaterThan    }
  "."        { mkToken TOperatorDot            }
  "..="      { mkToken TOperatorRangeInclusive }
  ".."       { mkToken TOperatorRangeExclusive }
  "&"        { mkToken TOperatorReference      }
  "!"        { mkToken TOperatorNot            }
  "-"        { mkToken TOperatorMinus          }
  "+"        { mkToken TOperatorPlus           }
  "/"        { mkToken TOperatorDiv            }
  "%"        { mkToken TOperatorMod            }
  "^"        { mkToken TOperatorPow            }
  "=="       { mkToken TOperatorEqual          }
  "!="       { mkToken TOperatorDiff           }
  ">="       { mkToken TOperatorGreaterOrEqual }
  "<="       { mkToken TOperatorLessOrEqual    }
  "&&"       { mkToken TOperatorBoolAnd        }
  "||"       { mkToken TOperatorBoolOr         }
  "+="       { mkToken TOperatorAssignPlus     }
  "-="       { mkToken TOperatorAssignMinus    }
  "*="       { mkToken TOperatorAssignMult     }
  "/="       { mkToken TOperatorAssignDiv      }
  "%="       { mkToken TOperatorAssignMod      }
  "^="       { mkToken TOperatorAssignPow      }
  "("        { mkToken TDelimiterParensOpen    }
  ")"        { mkToken TDelimiterParensClose   }
  "{"        { mkToken TDelimiterBracesOpen    }
  "}"        { mkToken TDelimiterBracesClose   }
  "["        { mkToken TDelimiterBracketsOpen  }
  "]"        { mkToken TDelimiterBracketsClose }

  [0-9]+           { mkDecimalLiteral                }
  "0x"[0-9A-Fa-f]+ { mkHexadecimalLiteral            }
  "'"              { mkCharLiteral                   }
  "                { mkStringLiteral                 }

  .                { mkIdentifier                    }


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

}
