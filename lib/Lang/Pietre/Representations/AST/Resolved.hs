{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lang.Pietre.Representations.AST.Resolved
  ( module Lang.Pietre.Representations.AST.Resolved
  , module Common
  ) where

import "this" Prelude

import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name

import Lang.Pietre.Representations.AST.Common as Common


--------------------------------------------------------------------------------
-- AST Representation

instance ASTRepresentation Resolved where
  type PathBodyType   Resolved = Role
  type ExpressionType Resolved = WithLocation Expression
  type ForInfoType    Resolved = ForInfo
  type LetInfoType    Resolved = LetInfo


--------------------------------------------------------------------------------
-- Resolved AST definitions

data Role
  = BuiltinType Name
  | Struct BaseName
  | Enum BaseName
  | Constant BaseName
  | Function BaseName
  | TypeAlias BaseName
  | TypeParameter BaseName Identifier
  | Placeholder
  | FunctionArgument Identifier (FunctionArgType PathInfo)
  | LetVariable Identifier

deriving instance ShowConstraints Resolved => Show Role
deriving instance LiftConstraints Resolved => Lift Role


--------------------------------------------------------------------------------
-- Re-exports

type Block           = CommonBlock           Resolved
type ConstInfo       = CommonConstInfo       Resolved
type Definition      = CommonDefinition      Resolved
type ElseInfo        = CommonElseInfo        Resolved
type Expression      = CommonExpression      Resolved
type ForInfo         = CommonForInfo         Resolved
type FunctionInfo    = CommonFunctionInfo    Resolved
type FunctionType    = CommonFunctionType    Resolved
type IfInfo          = CommonIfInfo          Resolved
type LetInfo         = CommonLetInfo         Resolved
type PathInfo        = CommonPathInfo        Resolved
type Statement       = CommonStatement       Resolved
type StructInfo      = CommonStructInfo      Resolved
type TypeAliasInfo   = CommonTypeAliasInfo   Resolved
type WhileInfo       = CommonWhileInfo       Resolved
