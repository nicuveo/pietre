module Lang.Pietre.Stages.Analysis.Instantiation where

import "this" Prelude

import Data.HashMap.Strict                qualified as M

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Monad


substituteTypes
  :: HashMap Identifier (PathInfo Resolved)
  -> PathInfo Resolved
  -> AnalysisM (PathInfo Resolved)
substituteTypes mappings info@PathInfo {..} = case _pathName of
  TypeParameter _ name -> M.lookup name mappings `onNothing` error "ICE"
  _                    -> pathParams (traverse $ substituteTypes mappings) info
