{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra                    (whenM)
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                    qualified as M
import Data.Set                               qualified as S
import Data.Tuple

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Symbol
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Diagnostic


--------------------------------------------------------------------------------
-- Monad

type AnalysisM = MaybeT (RWS AnalysisInfo [Diagnostic] AnalysisState)

type Scope = HashMap Path (NonEmpty Role)

type DefinitionCache = HashMap Name (WithLocation (Definition Resolved))
type SymbolCache     = HashMap Name Symbol
type FunctionCache   = HashMap Name (Scope, WithLocation (FunctionInfo Parsed))

data AnalysisInfo = AnalysisInfo
  { _infoModuleName         :: ModuleName
  , _infoStack              :: HashSet Name
  , _infoLocalDefinitions   :: HashMap Name (WithLocation (Definition Parsed))
  , _infoForeignDefinitions :: DefinitionCache
  , _infoForeignSymbols     :: SymbolCache
  , _infoForeignFunctions   :: FunctionCache
  , _infoTopLevelScope      :: Scope
  }

data AnalysisState = AnalysisState
  { _moduleDefinitions :: HashMap Name (Maybe (WithLocation (Definition Resolved)))
  , _moduleSymbols     :: SymbolCache
  , _moduleFunctions   :: FunctionCache
  , _moduleFunTypes    :: HashMap Name (FunctionType Resolved)
  , _moduleInstances   :: S.Set (Name, HashMap Identifier (PathInfo Resolved))
  , _moduleAnyError    :: Bool
  , _moduleNewError    :: Bool
  , _moduleContext     :: [AnalysisContext]
  }
  deriving Show

data AnalysisContext = AnalysisContext
  { _contextScope      :: Scope
  , _contextName       :: Name
  , _contextLocation   :: Location
  , _contextFunType    :: Maybe (PathInfo Resolved)
  , _contextParams     :: HashMap Identifier (PathInfo Resolved)
  , _contextWithinLoop :: Bool
  }
  deriving Show

makeLenses ''AnalysisInfo
makeLenses ''AnalysisState
makeLenses ''AnalysisContext

currentContext :: Lens' AnalysisState AnalysisContext
currentContext = moduleContext . unsafeHead
  where
    unsafeHead f = \case
      []     -> error "ICE"
      (c:cs) -> (:cs) <$> f c

currentScope :: Lens' AnalysisState Scope
currentScope = currentContext . contextScope

currentName :: Lens' AnalysisState Name
currentName = currentContext . contextName

currentLocation :: Lens' AnalysisState Location
currentLocation = currentContext . contextLocation

currentFunType :: Lens' AnalysisState (Maybe (PathInfo Resolved))
currentFunType = currentContext . contextFunType

currentParams :: Lens' AnalysisState (HashMap Identifier (PathInfo Resolved))
currentParams = currentContext . contextParams

currentlyWithinLoop :: Lens' AnalysisState Bool
currentlyWithinLoop = currentContext . contextWithinLoop

initialState :: AnalysisState
initialState = AnalysisState
  { _moduleDefinitions = M.empty
  , _moduleSymbols     = M.empty
  , _moduleFunctions   = M.empty
  , _moduleFunTypes    = M.empty
  , _moduleInstances   = S.empty
  , _moduleAnyError    = False
  , _moduleNewError    = False
  , _moduleContext     = []
  }

runAnalysis
  :: AnalysisInfo
  -> AnalysisM a
  -> ([Diagnostic], Maybe a)
runAnalysis info action = swap $ evalRWS (runMaybeT checkedAction) info initialState
  where
    checkedAction = action <* whenM (use moduleAnyError) abort

withContext
  :: Scope
  -> Name
  -> Location
  -> AnalysisM a
  -> AnalysisM (Maybe a)
withContext topLevelScope name defLocation action = do
  let context = AnalysisContext
        { _contextScope      = topLevelScope
        , _contextName       = name
        , _contextLocation   = defLocation
        , _contextFunType    = Nothing
        , _contextParams     = M.empty
        , _contextWithinLoop = False
        }
  moduleContext %= (context :)
  result <- try action
  moduleContext %= drop 1
  pure result

withNestedContext
  :: AnalysisM a
  -> AnalysisM (Maybe a)
withNestedContext action = do
  context <- use currentContext
  moduleContext %= (context :)
  result <- try action
  moduleContext %= drop 1
  pure result


--------------------------------------------------------------------------------
-- Error handling

instance MonadDiagnostic AnalysisM where
  report msg = do
    when (isErrorMsg msg) do
      moduleAnyError .= True
      moduleNewError .= True
    name <- use currentName
    loc  <- use currentLocation
    tell [Diagnostic (Just name) loc msg]

  abort = mzero

validate :: AnalysisM ()
validate = whenM (use moduleNewError) abort

ensure :: AnalysisM a -> AnalysisM a
ensure = (<* validate)

try :: AnalysisM a -> AnalysisM (Maybe a)
try action = MaybeT do
  result <- runMaybeT action
  moduleNewError .= False
  pure $ Just result
