module Lang.Pietre.Stages.Analysis.Validation.Instantiation where

import "this" Prelude


import Control.Lens                                 hiding (mapping, op)
import Control.Monad.Loops                          (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                    (hoistMaybe)
import Data.HashMap.Strict.Extra                    qualified as M
import Data.List                                    qualified as L
import Data.Ordered.Set                             qualified as OSet
import Data.Seq                                     qualified as Seq

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Resolved     as Resolved
import Lang.Pietre.Representations.AST.Validated    as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Expr
import Lang.Pietre.Stages.Analysis.Validation.Monad


instantiateAllSymbols
  :: Monad m
  => ValidateT m ()
instantiateAllSymbols = do
  originalRequests <- uses vsInstanceRequests
  void $ iterateUntilM Seq.null step originalRequests
  where
    step requests = do
      vsInstanceRequests .= Seq.empty
      traverse_ instantiate requests
      uses vsInstanceRequests

instantiate
  :: Monad m
  => FunctionInstantiationRequest
  -> ValidateT m ()
instantiate FunctionInstantiationRequest {..} = do
  let name = unimplemented
  alreadyInstantiated <- uses vsSymbols (M.member name)
