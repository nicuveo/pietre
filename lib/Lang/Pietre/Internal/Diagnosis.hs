module Lang.Pietre.Internal.Diagnosis where

import "this" Prelude

import Control.Monad.Trans.Control


data Diagnostic = Diagnostic
  { _diagnosticDeclaration :: Maybe BaseName
  , _diagnosticLocation    :: Location
  , _diagnosticMessage     :: Message
  } deriving Show

data Message
  = ErrorImportPath ModuleName
  | ErrorImportSymbol ModuleName Identifier
  | ErrorMultipleDeclaration Identifier (NonEmpty Location)
  | ErrorRoleNotFound Path
  | ErrorNotAType Role
  | ErrorNotAConst Role
  | ErrorNotAStruct Role
  | ErrorNotAValue Role
  | ErrorNotAFunction Role
  | ErrorAmbiguousPath Path (NonEmpty Role)
  | ErrorCyclicDefinition Name
  | ErrorIncorrectTypeParameterCount Name Int Int
  | ErrorDuplicatedTypeParameter Identifier
  | ErrorEnumDuplicatedEntry Identifier
  | ErrorWrongType [Type] Type
  | ErrorIncompatibleType HollowType Type
  | ErrorWrongCast (PathInfo Resolved) (PathInfo Resolved)
  | ErrorEnumOutOfBounds (EnumInfo Resolved) Int
  | ErrorStructMissingField (PathInfo Resolved) Identifier
  | ErrorStructDuplicatedField (PathInfo Resolved) Identifier
  | ErrorStructUnknownField (PathInfo Resolved) Identifier
  | ErrorStructAmbiguousType (PathInfo Resolved) Identifier
  | ErrorStructIncompatibleTypes (PathInfo Resolved) Identifier (NonEmpty (PathInfo Resolved))
  | ErrorFunctionAmbiguousType (PathInfo Resolved) Identifier
  | ErrorFunctionIncompatibleTypes (PathInfo Resolved) Identifier (NonEmpty (PathInfo Resolved))
  | ErrorFieldAccessNotAStruct Type
  | ErrorFieldAccessFieldNotFound Type Identifier
  | ErrorReservedIdentifier Identifier
  | ErrorPlaceholder Text
  | ErrorFunctionDuplicatedArg Identifier
  | ErrorBreakNotInLoop
  | ErrorContinueNotInLoop
  | ErrorFunctionCallWrongNumberOfArguments (PathInfo Resolved) Int Int
  | ErrorDivideByZero
  | ErrorNegativeExponent
  | ErrorReferenceNotLocalVariable (Expression Resolved)
  | ErrorFunctionCallArgExpectingReference Identifier
  | ErrorRValueAssignment (Expression Resolved)
  | ErrorIfExprNotBoolean (PathInfo Resolved)
  | ErrorWhileExprNotBoolean (PathInfo Resolved)
  | ErrorTypeParametersToTypeParameter Identifier
  | WarningNameShadow (NonEmpty Role) Identifier Role
  | WarningUnexpectedTopLevelExpression (Expression Resolved)
  deriving Show


newtype DiagnosisT m a = DiagnosisT (DiagnosisState -> (DiagnosisState, m (Maybe a)))
  deriving (Functor, Applicative, Monad, MonadTrans)

runDiagnosisT :: DiagnosisT m a -> m (Seq Diagnostic, Maybe a)
runDiagnosisT (DiagnosisT step) =
  let (DiagnosisState {..}, maybeResult) = step (DiagnosisState Seq.empty False)
  in pure $ if _dsAnyError
            then (_dsAllDiagnostics, Nothing)
            else (_dsAllDiagnostics, maybeResult)

runDiagnosis :: Diagnosis a -> (Seq Diagnostic, Maybe a)
runDiagnosis = runIdentity . runDiagnosisT

type Diagnosis = DiagnosisT Identity

data DiagnosisState = DiagnosisState
  { _dsAllDiagnostics :: Seq Diagnostic
  , _dsAnyError       :: Bool
  }

makeLenses ''DiagnosisState

class Monad m => MonadDiagnosis m where
  reportWarning :: Diagnostic -> m ()
  reportError   :: Diagnostic -> m a
  try           :: m a -> m (Maybe a)
  ensure        :: Maybe a -> m a

instance Monad m => MonadDiagnosis (DiagnosisT m) where
  reportWarning d =
    DiagnosisT \s -> (s & dsAllDiagnostics %~ (:|> d), pure $ Just ())
  reportError d = DiagnosisT \DiagnostisState {..} ->
    (DiagnosisState (_dsAllDiagnostics :|> d) True, pure Nothing)
  try (DiagnosisT step) = DiagnosisT (fmap Just . step)
  ensure ma = DiagnosisT (, pure ma)

instance (MonadDiagnosis m, MonadTransControl t) => MonadDiagnosis t m where
  reportWarning = lift . reportWarning
  reportError = lift . reportError
  ensure = lift . ensure
  try ma = liftWith \run -> try (run ma)


tryNested :: MonadDiagnosis m => m a -> Compose m Maybe a
tryNested = Compose . try

ensureNested :: MonadDiagnosis m => Compose m Maybe a -> m a
ensureNested = getCompose >=> ensure
