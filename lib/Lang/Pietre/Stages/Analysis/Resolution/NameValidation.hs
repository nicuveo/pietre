module Lang.Pietre.Stages.Analysis.Resolution.NameValidation where

import "this" Prelude

import Control.Monad.Extra                          (whenJustM)

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Stages.Analysis.Resolution.Monad


findDuplicates
  :: HasCallStack
  => [Identifier]
  -> [Identifier]
findDuplicates =
  mapMaybe checkGroup . group . sort
  where
    checkGroup = \case
      [] ->
        reportICE "type parameter analysis" "found empty group" []
      (identifier:_:_) ->
        Just identifier
      [_] ->
        Nothing

validateBinding
  :: Identifier
  -> Role
  -> Resolve ()
validateBinding identifier role = do
  when (isReserved identifier) $
    fatal $ ErrorReservedIdentifier identifier
  whenJustM (lookupName $ pure identifier) \names ->
    warn $ WarningNameShadow names identifier role
