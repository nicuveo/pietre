{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Interface where

import "this" Prelude

import Control.Lens.TH
import Lang.Pietre.Representations.AST.Parsed    as Parsed
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


type FunctionCache   = HashMap BaseName (WithLocation Resolved.FunctionInfo)
type DefinitionCache = HashMap BaseName Validated.Definition
type SymbolCache     = HashMap Name     Validated.FunctionInfo

data Interface = Interface
  { _interfaceDependencies :: Seq Import
  , _interfaceExported     :: HashMap Identifier Role
  , _interfaceDefinitions  :: DefinitionCache
  , _interfaceFunctions    :: FunctionCache
  , _interfaceSymbols      :: SymbolCache
  } deriving (Show, Lift)

makeLenses ''Interface
