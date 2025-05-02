module Lang.Pietre.Stages.Analysis.Diagnostic where

import "this" Prelude


data Diagnostic
  = WarningShadow
  | ErrorDuplicateName
  deriving Show
