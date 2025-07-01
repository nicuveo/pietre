module Lang.Pietre.Stages.Analysis.Core where

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Stages.Analysis.Monad

analyzeDefinition
  :: WithLocation (Definition Parsed)
  -> AnalysisM (Definition Resolved)
