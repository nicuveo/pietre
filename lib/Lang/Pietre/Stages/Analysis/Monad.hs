{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                    qualified as M
import Data.HashSet                           qualified as S
import Data.Tuple

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Diagnostic


type AnalysisM = RWS AnalysisInfo [Diagnostic] AnalysisContext

data AnalysisInfo = AnalysisInfo
  { _infoModuleName    :: ModuleName
  , _infoStack         :: HashSet Name
  , _infoTopLevelNames :: HashMap Path (NonEmpty Name)
  , _infoLocals        :: HashMap Name (WithLocation (Declaration Parsed))
  }

data AnalysisContext = AnalysisContext
  { _contextDeclarations :: HashMap Name (WithLocation (Declaration Resolved))
  , _contextNames        :: HashMap Path (NonEmpty Name)
  , _contextLocation     :: Location
  , _contextFunType      :: PathInfo Resolved
  , _contextWithinLoop   :: Bool
  }
  deriving Show

runAnalysis
  :: ModuleName
  -> AnalysisM a
  -> ([Diagnostic], a)
runAnalysis moduleName action = swap $ evalRWS
  action
  (AnalysisInfo moduleName S.empty (M.map pure $ M.fromList builtins) M.empty)
  (AnalysisContext M.empty M.empty (initialLocation "") UnitType False)


makeLenses ''AnalysisInfo
makeLenses ''AnalysisContext


resetState :: MaybeT AnalysisM ()
resetState = do
  topLevelNames <- view infoTopLevelNames
  contextNames      .= topLevelNames
  contextFunType    .= UnitType
  contextWithinLoop .= False
