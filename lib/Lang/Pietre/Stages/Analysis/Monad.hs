{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                    qualified as M
import Data.Tuple

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Diagnostic


type AnalysisM = RWS AnalysisInfo [Diagnostic] AnalysisContext

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
  }
  deriving Show

initialContext :: AnalysisContext
initialContext = AnalysisContext M.empty M.empty (initialLocation "") UnitType False

runAnalysis
  :: AnalysisInfo
  -> AnalysisM a
  -> ([Diagnostic], a)
runAnalysis info action = swap $ evalRWS action info initialContext

makeLenses ''AnalysisInfo
makeLenses ''AnalysisContext


resetState :: MaybeT AnalysisM ()
resetState = do
  topLevelScope <- view infoTopLevelScope
  contextScope      .= topLevelScope
  contextFunType    .= UnitType
  contextWithinLoop .= False
