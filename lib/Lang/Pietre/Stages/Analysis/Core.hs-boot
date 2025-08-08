module Lang.Pietre.Stages.Analysis.Core where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Monad

analyzeDefinition
  :: WithLocation (Definition Parsed)
  -> AnalysisM (Maybe (Definition Resolved))

analyzeFunction
  :: FunctionInfo Parsed
  -> AnalysisM (FunctionInfo Resolved)

setTypeParameters
  :: [Identifier]
  -> AnalysisM ()
