{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Monad where

import "this" Prelude

import Control.Lens                           (makeLenses)
import Control.Monad.RWS.Strict
import Data.Tuple

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Diagnostic


type AnalyzeM = MaybeT (RWS Context [Diagnostic] Scope)

data Context = Context
  { _contextModuleName :: ModuleName
  , _contextLocation   :: Location
  }

data Scope = Scope
  { _scopeNames :: HashMap Identifier (NonEmpty (Name, Maybe (Declaration Resolved)))
  }

runAnalysis
  :: ModuleName
  -> Scope
  -> AnalyzeM a
  -> ([Diagnostic], Maybe a)
runAnalysis moduleName scope action = swap $ evalRWS
  (runMaybeT action)
  (Context moduleName $ initialLocation "")
  scope


makeLenses ''Context
makeLenses ''Scope
