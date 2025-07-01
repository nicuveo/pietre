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

data AnalysisInfo = AnalysisInfo
  { _infoModuleName         :: ModuleName
  , _infoStack              :: HashSet Name
  , _infoLocalDefinitions   :: HashMap Name (WithLocation (Definition Parsed))
  , _infoForeignDefinitions :: HashMap Name (WithLocation (Definition Resolved))
  , _infoTopLevelScope      :: HashMap Path (NonEmpty Role)
  }

data AnalysisContext = AnalysisContext
  { _contextCache      :: HashMap Name (WithLocation (Definition Resolved))
  , _contextScope      :: HashMap Path (NonEmpty Role)
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
initialContext = AnalysisContext M.empty M.empty (initialLocation "") UnitType False False False

runAnalysis
  :: AnalysisInfo
  -> AnalysisM a
  -> ([Diagnostic], Maybe a)
runAnalysis info action = swap $ evalRWS (runMaybeT checkedAction) info initialContext
  where
    checkedAction = action <* whenM (use contextAnyError) abort

resetState :: AnalysisM ()
resetState = do
  topLevelScope <- view infoTopLevelScope
  contextScope      .= topLevelScope
  contextFunType    .= UnitType
  contextWithinLoop .= False
  contextNewError   .= False


--------------------------------------------------------------------------------
-- Error handling

report :: Diagnostic -> AnalysisM ()
report diag = do
  when (isError diag) do
    contextAnyError .= True
    contextNewError .= True
  tell [diag]

fatal :: Diagnostic -> AnalysisM a
fatal d = report d >> abort

abort :: AnalysisM a
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
