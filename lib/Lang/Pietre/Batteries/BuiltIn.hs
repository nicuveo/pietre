{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Batteries.BuiltIn where

import "this" Prelude

import Data.HashSet                       qualified as S
import Data.List.NonEmpty                 qualified as NE

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


builtins :: [(Path, Role)]
builtins =
  [ (pure "int",   _pathName IntType)
  , (pure "char",  _pathName CharType)
  , (pure "bool",  _pathName BoolType)
  , (pure "()",    _pathName UnitType)
  , (pure "!void", _pathName VoidType)
  , (pure "_",     Placeholder)
  ]

reserved :: HashSet Identifier
reserved = S.fromList $ map (NE.head . fst) builtins

isReserved :: Identifier -> Bool
isReserved = flip S.member reserved

pattern IntName  :: Name
pattern IntName  = Name ("int"   :| []) []
pattern CharName :: Name
pattern CharName = Name ("char"  :| []) []
pattern BoolName :: Name
pattern BoolName = Name ("bool"  :| []) []
pattern UnitName :: Name
pattern UnitName = Name ("()"    :| []) []
pattern VoidName :: Name
pattern VoidName = Name ("!void" :| []) []

pattern IntType  :: PathInfo Resolved
pattern IntType  = PathInfo (BuiltinType IntName)  []
pattern CharType :: PathInfo Resolved
pattern CharType = PathInfo (BuiltinType CharName) []
pattern BoolType :: PathInfo Resolved
pattern BoolType = PathInfo (BuiltinType BoolName) []
pattern UnitType :: PathInfo Resolved
pattern UnitType = PathInfo (BuiltinType UnitName) []
pattern VoidType :: PathInfo Resolved
pattern VoidType = PathInfo (BuiltinType VoidName) []
pattern PlaceholderType :: PathInfo Resolved
pattern PlaceholderType = PathInfo Placeholder []

pattern IntExpression :: Int -> TypedExpression
pattern IntExpression x = RValueExpression Pure IntType (IntLiteralExpr x)
pattern CharExpression :: Char -> TypedExpression
pattern CharExpression x = RValueExpression Pure CharType (CharLiteralExpr x)
pattern BoolExpression :: Bool -> TypedExpression
pattern BoolExpression x = RValueExpression Pure BoolType (BoolLiteralExpr x)
