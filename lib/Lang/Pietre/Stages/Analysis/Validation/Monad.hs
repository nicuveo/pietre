module Lang.Pietre.Stages.Analysis.Validation.Monad where

import "this" Prelude

type Validate m = ReaderT ValidateInfo (StateT ValidateContext m)

data ValidateInfo = ValidateInfo
  { _viModuleName  :: ModuleName
  , _viDefinitions :: HashMap BaseName (Definition Resolved)
  }

data ValidateState = ValidateState
  { _vsSymbols    :: HashMap Name Function
  , _vsTypeCache  :: HashMap Name Type
  , _vsValueCache :: HashMap Name (Typed Expression)
  , _vsInstances  :: S.Set (BaseName, [Type])
  , _vsContext    :: [ValidateContext]
  , _vsValidated  :: S.HashSet Name
  }
  deriving Show

data ValidateContext = ValidateContext
  { _contextName     :: BaseName
  , _contextLocation :: Location
  , _contextFunType  :: Maybe (PathInfo Resolved)
  , _contextParams   :: HashMap Identifier Type
  }
  deriving Show

makeLenses ''AnalysisInfo
makeLenses ''AnalysisState
makeLenses ''AnalysisContext

currentContext :: Lens' AnalysisState AnalysisContext
currentContext = moduleContext . unsafeHead
  where
    unsafeHead f = \case
      (c:cs) -> (:cs) <$> f c
      []     -> reportICE "analysis" "context stack empty" []

currentName :: Lens' AnalysisState Name
currentName = currentContext . contextName

currentLocation :: Lens' AnalysisState Location
currentLocation = currentContext . contextLocation

currentFunType :: Lens' AnalysisState (Maybe (PathInfo Resolved))
currentFunType = currentContext . contextFunType

currentParams :: Lens' AnalysisState (HashMap Identifier (PathInfo Resolved))
currentParams = currentContext . contextParams

withContext
  :: Name
  -> Location
  -> AnalysisM a
  -> AnalysisM a
withContext name defLocation action = do
  let context = AnalysisContext
        { _contextName       = name
        , _contextLocation   = defLocation
        , _contextFunType    = Nothing
        , _contextParams     = M.empty
        }
  moduleContext %= (context :)
  result <- try action
  moduleContext %= drop 1
  ensure result
