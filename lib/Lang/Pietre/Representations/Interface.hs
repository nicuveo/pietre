{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Interface where

import "this" Prelude

import Control.Lens.TH
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Symbol


type Scope = HashMap Path (NonEmpty Role)

type DefinitionCache = HashMap Name (WithLocation (Definition Resolved))
type SymbolCache     = HashMap Name Symbol
type FunctionCache   = HashMap Name (Scope, WithLocation (FunctionInfo Parsed))

data Interface = Interface
  { _interfaceExported    :: HashSet Identifier
  , _interfaceDefinitions :: DefinitionCache
  , _interfaceSymbols     :: SymbolCache
  , _interfaceFunctions   :: FunctionCache
  }

makeLenses ''Interface
