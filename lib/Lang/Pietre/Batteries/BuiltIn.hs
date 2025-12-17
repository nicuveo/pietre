{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Batteries.BuiltIn where

import "this" Prelude

import Data.HashSet                             qualified as S
import Data.List.NonEmpty                       qualified as NE

import Lang.Pietre.Representations.AST.Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Name


builtins :: [(Path, Role)]
builtins =
  [ (pure "int",   BuiltinType IntName)
  , (pure "char",  BuiltinType CharName)
  , (pure "bool",  BuiltinType BoolName)
  , (pure "()",    BuiltinType UnitName)
  , (pure "!void", BuiltinType VoidName)
  , (pure "_",     Placeholder)
  ]

reserved :: HashSet Identifier
reserved = S.fromList $ map (NE.head . fst) builtins

isReserved :: Identifier -> Bool
isReserved = flip S.member reserved

pattern IntName  :: Name
pattern IntName  = Name (BaseName ("%builtin%" :| []) "int")   []
pattern CharName :: Name
pattern CharName = Name (BaseName ("%builtin%" :| []) "char")  []
pattern BoolName :: Name
pattern BoolName = Name (BaseName ("%builtin%" :| []) "bool")  []
pattern UnitName :: Name
pattern UnitName = Name (BaseName ("%builtin%" :| []) "()")    []
pattern VoidName :: Name
pattern VoidName = Name (BaseName ("%builtin%" :| []) "!void") []
