{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Validation.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict.Extra                 qualified as M
import Data.HashSet                              qualified as S
import Data.Sequence                             qualified as Seq
import Data.Set.Ordered                          (OSet)
import Data.Set.Ordered                          qualified as OSet

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


type Validate = DiagnosisT (ReaderT ValidateInfo (State ValidateState))

runValidate
  :: MonadDiagnosis m
  => ValidateInfo
  -> Validate a
  -> m (ValidateState, a)
runValidate info action = do
  let ((diagnostics, result), validateState) =
        action
        & runDiagnosisT
        & flip runReaderT info
        & flip runState initialState
  subsume (diagnostics, (validateState, ) <$> result)
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
  { _viDefinitions      :: DefinitionCache
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
  , _firDefinition :: WithLocation Resolved.FunctionInfo
  , _firFunType    :: FunctionTypeInfo ConcreteFunctor
  , _firParams     :: [ConcreteType]
  }
  deriving Show

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

currentContext :: HasCallStack => Lens' ValidateState ValidateContext
currentContext = vsContext . unsafeHead
  where
    unsafeHead f = \case
      (c:cs) -> (:cs) <$> f c
      []     -> reportICE "analysis" "context stack empty" []

currentName :: Lens' ValidateState BaseName
currentName = currentContext . contextName

currentLocation :: Lens' ValidateState Location
currentLocation = currentContext . contextLocation

currentFunType :: Lens' ValidateState ConcreteType
currentFunType = currentContext . contextFunType

currentParams :: Lens' ValidateState (HashMap (BaseName, Identifier) ConcreteType)
currentParams = currentContext . contextParams

currentVariables :: Lens' ValidateState (HashMap Identifier ConcreteType)
currentVariables = currentContext . contextVariables

withContext
  :: BaseName
  -> Location
  -> Validate a
  -> Validate a
withContext name defLocation action = do
  let context = ValidateContext
        { _contextName      = name
        , _contextLocation  = defLocation
        , _contextFunType   = UnitType
        , _contextParams    = M.empty
        , _contextVariables = M.empty
        }
  bracket_
    (vsContext %= (context :))
    (vsContext %= drop 1)
    action

fatal :: Message -> Validate a
fatal message = do
  declName <- use currentName
  declLocation <- use currentLocation
  reportError $ Diagnostic (Just declName) declLocation message

warn :: Message -> Validate ()
warn message = do
  declName <- use currentName
  declLocation <- use currentLocation
  reportWarning $ Diagnostic (Just declName) declLocation message
