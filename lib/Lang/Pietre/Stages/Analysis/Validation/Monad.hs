module Lang.Pietre.Stages.Analysis.Validation.Monad where

import "this" Prelude

import Data.Set                                  qualified as S
import Data.Set.Ordered                          (OSet)

import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated


type ValidateT m = ReaderT ValidateInfo (StateT ValidateContext m)

runValidateT :: ValidateInfo -> ValidateT m a -> m (ValidateState, a)
runValidateT info action = action
  & flip runReaderT info
  & flip runStateT initialState
  & fmap swap
  where
    initialState = ValidateState
      { _vsDefinitions      = M.empty
      , _vsFunctions        = M.empty
      , _vsInstanceRequests = Seq.empty
      , _vsContext          = []
      , _vsValidated        = S.empty
      , _vsDefinitionStack  = OSet.empty
      }

data ValidateInfo = ValidateInfo
  { _viModuleName       :: ModuleName
  , _viDefinitions      :: DefinitionCache
  , _viFunctions        :: FunctionCache
  , _viSymbols          :: SymbolCache
  , _viLocalDefinitions :: HashMap BaseName (WithLocation Resolved.Definition)
  }

data ValidateState = ValidateState
  { _vsDefinitions      :: DefinitionCache
  , _vsFunctions        :: FunctionCache
  , _vsInstanceRequests :: Seq FunctionInstantiationRequest
  , _vsContext          :: [ValidateContext]
  , _vsValidated        :: HashSet BaseName
  , _vsDefinitionStack  :: OSet BaseName
  }
  deriving Show

data FunctionInstantiationRequest = FunctionInstantiationRequest
  { _firBaseName   :: BaseName
  , _firDefinition :: Resolved.FunctionInfo
  , _firFunType    :: FunctionTypeInfo ConcreteFunctor
  , _firParams     :: [ConcreteType]
  }

data ValidateContext = ValidateContext
  { _contextName      :: BaseName
  , _contextLocation  :: Location
  , _contextFunType   :: ConcreteType
  , _contextParams    :: HashMap (BaseName, Identifier) ConcreteType
  , _contextVariables :: HashMap Identifier ConcreteType
  }
  deriving Show

makeLenses ''ValidateInfo
makeLenses ''ValidateState
makeLenses ''ValidateContext

currentContext :: Lens' ValidateState ValidateContext
currentContext = moduleContext . unsafeHead
  where
    unsafeHead f = \case
      (c:cs) -> (:cs) <$> f c
      []     -> reportICE "analysis" "context stack empty" []

currentName :: Lens' ValidateState BaseName
currentName = currentContext . contextName

currentLocation :: Lens' ValidateState Location
currentLocation = currentContext . contextLocation

currentFunType :: Lens' ValidateState (Maybe ConcreteType)
currentFunType = currentContext . contextFunType

currentParams :: Lens' ValidateState (HashMap (BaseName, Identifier) ConcreteType)
currentParams = currentContext . contextParams

currentVariables :: Lens' ValidateState (HashMap Identifier ConcreteType)
currentVariables = currentContext . contextVariables

withContext
  :: BaseName
  -> Location
  -> ValidateT a
  -> ValidateT a
withContext name defLocation action = do
  let context = AnalysisContext
        { _contextName      = name
        , _contextLocation  = defLocation
        , _contextFunType   = UnitType
        , _contextParams    = M.empty
        , _contextVariables = M.empty
        }
  moduleContext %= (context :)
  result <- try action
  moduleContext %= drop 1
  ensure result


fatal :: MonadDiagnosis m => Message -> ValidationT m a
fatal diagnosticMessage = do
  baseName <- use currentName
  location <- use currentLocation
  reportError $ Diagnostic baseName location diagnosticMessage

warn :: MonadDiagnosis m => Message -> ValidationT m a
warn diagnosticMessage = do
  baseName <- use currentName
  location <- use currentLocation
  reportWarning $ Diagnostic baseName location diagnosticMessage
