module Lang.Pietre.Representations.Tokens where

data Token
  = TKeywordAs
  | TKeywordBreak
  | TKeywordConst
  | TKeywordContinue
  | TKeywordElse
  | TKeywordEnum
  | TKeywordFalse
  | TKeywordFn
  | TKeywordFor
  | TKeywordIf
  | TKeywordIn
  | TKeywordLet
  | TKeywordReturn
  | TKeywordStruct
  | TKeywordTrue
  | TKeywordType
  | TKeywordUse
  | TKeywordWhile

  | TOperatorSemicolon
  | TOperatorType
  | TOperatorStar
  | TOperatorComma
  | TOperatorAssign
  | TOperatorColon
  | TOperatorArrow
  | TOperatorLessThan
  | TOperatorGreaterThan
  | TOperatorDot
  | TOperatorRangeInclusive
  | TOperatorRangeExclusive
  | TOperatorReference
  | TOperatorNot
  | TOperatorMinus
  | TOperatorPlus
  | TOperatorDiv
  | TOperatorMod
  | TOperatorPow
  | TOperatorEqual
  | TOperatorDiff
  | TOperatorGreaterOrEqual
  | TOperatorLessOrEqual
  | TOperatorBoolAnd
  | TOperatorBoolOr
  | TOperatorAssignPlus
  | TOperatorAssignMinus
  | TOperatorAssignMult
  | TOperatorAssignDiv
  | TOperatorAssignMod
  | TOperatorAssignPow

  | TDelimiterParensOpen
  | TDelimiterParensClose
  | TDelimiterBracesOpen
  | TDelimiterBracesClose
  | TDelimiterBracketsOpen
  | TDelimiterBracketsClose

  | TLiteralString Text
  | TLiteralChar   Char
  | TLiteralInt    Int

  | TIdentifier    Identifier

  | TEOF

  deriving Show
