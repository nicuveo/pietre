{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Representations.AST where

import "this" Prelude

import Control.Lens
import Data.Kind
import Data.List.NonEmpty                     qualified as NE
import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name



data Parsed

instance ASTRepresentation Parsed where
  type NameType Parsed = Path

data Module = Module
  { _modImports     :: [Import]
  , _modDefinitions :: [WithLocation (Definition Parsed)]
  }

deriving instance Show Module

instance Semigroup Module where
  Module imports1 decls1 <> Module imports2 decls2 =
    Module (imports1 <> imports2) (decls1 <> decls2)

instance Monoid Module where
  mempty = Module [] []


data Import = Import
  { _importPath :: NonEmpty Identifier
  , _importType :: ImportType
  }
  deriving Show

data ImportType
  = Qualified  (Maybe Identifier)
  | Specific   (NonEmpty Identifier)
  | Exhaustive
  deriving Show
