{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lang.Pietre.Representations.AST.Parsed
  ( module Lang.Pietre.Representations.AST.Parsed
  , module Common
  ) where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Representations.AST.Common as Common
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


--------------------------------------------------------------------------------
-- AST Representation

instance ASTRepresentation Parsed where
  type PathBodyType   Parsed = Path
  type ExpressionType Parsed = WithLocation Expression
  type ForInfoType    Parsed = ForInfo
  type LetInfoType    Parsed = LetInfo


--------------------------------------------------------------------------------
-- Parsed AST definitions

data Module = Module
  { _modImports     :: [WithLocation Import]
  , _modDefinitions :: [WithLocation Definition]
  } deriving (Show)

instance Semigroup Module where
  Module imports1 decls1 <> Module imports2 decls2 =
    Module (imports1 <> imports2) (decls1 <> decls2)

instance Monoid Module where
  mempty = Module [] []


data Import = Import
  { _importPath :: ModuleName
  , _importType :: ImportType
  }
  deriving (Show, Lift)

data ImportType
  = Qualified  (Maybe Identifier)
  | Specific   (NonEmpty Identifier)
  | Exhaustive
  deriving (Show, Lift)


--------------------------------------------------------------------------------
-- Re-exports

type Block           = CommonBlock           Parsed
type ConstInfo       = CommonConstInfo       Parsed
type Definition      = CommonDefinition      Parsed
type ElseInfo        = CommonElseInfo        Parsed
type Expression      = CommonExpression      Parsed
type ForInfo         = CommonForInfo         Parsed
type FunctionInfo    = CommonFunctionInfo    Parsed
type FunctionType    = CommonFunctionType    Parsed
type IfInfo          = CommonIfInfo          Parsed
type LetInfo         = CommonLetInfo         Parsed
type PathInfo        = CommonPathInfo        Parsed
type Statement       = CommonStatement       Parsed
type StructInfo      = CommonStructInfo      Parsed
type TypeAliasInfo   = CommonTypeAliasInfo   Parsed
type WhileInfo       = CommonWhileInfo       Parsed


--------------------------------------------------------------------------------
-- Lenses

makeLenses ''Module
makeLenses ''Import

makePrisms ''ImportType
