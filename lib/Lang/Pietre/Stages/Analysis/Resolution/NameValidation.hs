module Lang.Pietre.Stages.Analysis.Resolution.Namevalidation where

import "this" Prelude

import Control.Lens                                 hiding (mapping, op)
import Control.Monad.Loops                          (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                    (hoistMaybe)
import Data.HashMap.Strict.Extra                    qualified as M
import Data.HashSet                                 qualified as S
import Data.Set                                     qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Parsed       as Parsed
import Lang.Pietre.Representations.AST.Resolved     as Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Resolution.Monad


findDuplicates
  :: [Identifier]
  -> [Identifier]
findDuplicates =
  mapMaybe checkGroup . group . sort
  where
    checkGroup = \case
      [] ->
        reportICE "type parameter analysis" "found empty group" []
      (identifier:_:_) ->
        Just identifier
      [identifier] ->
        Nothing

validateBinding
  :: Monad m
  => Identifier
  -> Role
  -> ResolveT m ()
validateBinding identifier role = do
  when (isReserved identifier) $
    report $ ErrorReservedIdentifier identifier
  whenJustM (lookupName $ pure identifier) \names ->
    warn $ WarningNameShadow names identifier role
