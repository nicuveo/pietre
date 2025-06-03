{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Batteries.BuiltIn where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Name


builtins :: [(Path, Name)]
builtins =
  [ (pure "int",  _pathName IntType)
  , (pure "char", _pathName CharType)
  , (pure "bool", _pathName BoolType)
  ]

pattern IntType  :: PathInfo Resolved
pattern IntType  = PathInfo (BuiltinType "int")  []
pattern CharType :: PathInfo Resolved
pattern CharType = PathInfo (BuiltinType "char") []
pattern BoolType :: PathInfo Resolved
pattern BoolType = PathInfo (BuiltinType "bool") []
