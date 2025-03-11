{

module Lang.Pietre.Stages.Parsing.Lexer where

import "this" Prelude

import Data.Char (digitToInt)
import Data.Text qualified as T
import Lang.Pietre.Stages.Parsing.Monad
import Lang.Pietre.Representations.Tokens

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
  "\""             { mkStringLiteral                 }

  $white*          { mkIdentifier                    }


{

mkToken :: Token -> Location -> Text -> Parser (WithLocation Token)
mkToken t l _ = pure (l, t)

mkDecimalLiteral :: Location -> Text -> Parser (WithLocation Token)
mkDecimalLiteral l t = pure (l, TLiteralInt $ read $ T.unpack t)

mkHexadecimalLiteral :: Location -> Text -> Parser (WithLocation Token)
mkHexadecimalLiteral l t = pure (l, TLiteralInt $ foldl' readHex 0 $ T.unpack $ T.drop 2 t)

readHex :: Int -> Char -> Int
readHex accum c = 16 * accum + digitToInt c

mkCharLiteral :: Location -> Text -> Parser (WithLocation Token)
mkCharLiteral location _ = do
  c <- readStringChar
  expect '\''
  pure (location, TLiteralChar c)

mkStringLiteral :: Location -> Text -> Parser (WithLocation Token)
mkStringLiteral location _ = do
  s <- readStringChars
  pure (location, TLiteralString s)
  where
    readStringChars = do
      tryRead '"' >>= \case
        Just _  -> pure ""
        Nothing -> do
          c <- readStringChar
          fmap (c:) $ readStringChars

}
