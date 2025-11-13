{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Interface where

import "this" Prelude

import Control.Lens.TH
import Lang.Pietre.Representations.AST.Resolved qualified
import Lang.Pietre.Representations.AST.Validated qualified
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


type FunctionCache   = HashMap BaseName (WithLocation (Resolved.FunctionInfo))
type DefinitionCache = HashMap BaseName (Validated.Definition)
type SymbolCache     = HashMap Name     (Validated.FunctionInfo)

data Interface = Interface
  { _interfaceName        :: ModuleName
  , _interfaceExported    :: HashMap Identifier Role
  , _interfaceDefinitions :: DefinitionCache
  , _interfaceFunctions   :: FunctionCache
  , _interfaceSymbols     :: SymbolCache
  } deriving Show


makeLenses ''Interface
