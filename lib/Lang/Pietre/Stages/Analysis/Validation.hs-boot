module Lang.Pietre.Stages.Analysis.Validation where

import "this" Prelude

import Lang.Pietre.Representations.AST.Resolved     qualified as Resolved
import Lang.Pietre.Representations.AST.Validated    qualified as Validated
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Monad


validateDefinition
  :: BaseName
  -> WithLocation Resolved.Definition
  -> Validate (Maybe Validated.Definition)
