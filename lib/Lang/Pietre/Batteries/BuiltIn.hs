{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Batteries.BuiltIn where

import "this" Prelude

import Data.HashSet                       qualified as S
import Data.List.NonEmpty                 qualified as NE

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


builtins :: [(Path, Name)]
builtins =
  [ (pure "int",  _pathName IntType)
  , (pure "char", _pathName CharType)
  , (pure "bool", _pathName BoolType)
  ]

reserved :: HashSet Identifier
reserved = S.fromList $ map (NE.head . fst) builtins

isReserved :: Identifier -> Bool
isReserved = flip S.member reserved

pattern IntType  :: PathInfo Resolved
pattern IntType  = PathInfo (BuiltinType "int")  []
pattern CharType :: PathInfo Resolved
pattern CharType = PathInfo (BuiltinType "char") []
pattern BoolType :: PathInfo Resolved
pattern BoolType = PathInfo (BuiltinType "bool") []
