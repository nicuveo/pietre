{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra                    (whenM)
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                    qualified as M
import Data.Tuple

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Diagnostic


--------------------------------------------------------------------------------
-- Monad

type AnalysisM = MaybeT (RWS AnalysisInfo [Diagnostic] AnalysisContext)

type Scope = HashMap Path (NonEmpty Role)

data AnalysisInfo = AnalysisInfo
  { _infoModuleName         :: ModuleName
  , _infoStack              :: HashSet Name
  , _infoLocalDefinitions   :: HashMap Name (WithLocation (Definition Parsed))
  , _infoForeignDefinitions :: HashMap Name (WithLocation (Definition Resolved))
  , _infoTopLevelScope      :: Scope
  }

data AnalysisContext = AnalysisContext
  { _contextCache      :: HashMap Name (Maybe (WithLocation (Definition Resolved)))
  , _contextScope      :: Scope
  , _contextCurrent    :: Name
  , _contextLocation   :: Location
  , _contextFunType    :: PathInfo Resolved
  , _contextWithinLoop :: Bool
  , _contextAnyError   :: Bool
  , _contextNewError   :: Bool
  }
  deriving Show

makeLenses ''AnalysisInfo
makeLenses ''AnalysisContext

initialContext :: AnalysisContext
initialContext = AnalysisContext
  { _contextCache      = M.empty
  , _contextScope      = M.empty
  , _contextCurrent    = Name (pure $ error "ICE") []
  , _contextLocation   = initialLocation ""
  , _contextFunType    = UnitType
  , _contextWithinLoop = False
  , _contextAnyError   = False
  , _contextNewError   = False
  }

runAnalysis
  :: AnalysisInfo
  -> AnalysisM a
  -> ([Diagnostic], Maybe a)
runAnalysis info action = swap $ evalRWS (runMaybeT checkedAction) info initialContext
  where
    checkedAction = action <* whenM (use contextAnyError) abort

withLocalState
  :: Scope
  -> AnalysisM a
  -> AnalysisM (Maybe a)
withLocalState topLevelScope action = do
  previousScope      <- use contextScope
  previousFunType    <- use contextFunType
  previousWithinLoop <- use contextWithinLoop
  previousNewError   <- use contextNewError
  contextScope      .= topLevelScope
  contextFunType    .= UnitType
  contextWithinLoop .= False
  contextNewError   .= False
  result            <- try action
  contextScope      .= previousScope
  contextFunType    .= previousFunType
  contextWithinLoop .= previousWithinLoop
  contextNewError   .= previousNewError
  pure result


--------------------------------------------------------------------------------
-- Error handling

instance MonadDiagnostic AnalysisM where
  report diag = do
    when (isError diag) do
      contextAnyError .= True
      contextNewError .= True
    tell [diag]

  abort = mzero

validate :: AnalysisM ()
validate = whenM (use contextNewError) abort

ensure :: AnalysisM a -> AnalysisM a
ensure = (<* validate)

try :: AnalysisM a -> AnalysisM (Maybe a)
try action = MaybeT do
  result <- runMaybeT action
  contextNewError .= False
  pure $ Just result
