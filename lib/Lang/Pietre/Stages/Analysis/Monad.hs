{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Monad where

import "this" Prelude

import Control.Lens                           (makeLenses)
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
  , _infoLocation      :: Location
  , _infoNames         :: HashMap Path (NonEmpty Name)
  , _infoStack         :: HashSet Name
  , _infoTopLevelNames :: HashMap Path (NonEmpty Name)
  }

data AnalysisContext = AnalysisContext
  { _contextLocals       :: HashMap Name (WithLocation (Declaration Parsed))
  , _contextDeclarations :: HashMap Name (WithLocation (Declaration Resolved))
  }
  deriving Show

runAnalysis
  :: ModuleName
  -> AnalysisM a
  -> ([Diagnostic], a)
runAnalysis moduleName action = swap $ evalRWS
  action
  (AnalysisInfo moduleName (initialLocation "") M.empty S.empty (M.map pure $ M.fromList builtins))
  (AnalysisContext M.empty M.empty)


makeLenses ''AnalysisInfo
makeLenses ''AnalysisContext
