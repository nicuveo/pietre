{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Internal.Diagnosis where

import "this" Prelude

import Control.Lens                              hiding (mapping, op)
import Control.Monad.Catch                       (MonadCatch, MonadMask,
                                                  MonadThrow)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe                 (hoistMaybe)
import Data.Functor.Compose
import Data.Sequence                             qualified as Seq

import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


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
  | ErrorNotAStruct PartialType
  | ErrorNotAValue Role
  | ErrorNotAnLValue Role
  | ErrorNotAFunctionRole Role
  | ErrorNotAFunctionType ConcreteType
  | ErrorAmbiguousPath Path (NonEmpty Role)
  | ErrorCyclicDefinition BaseName [BaseName]
  | ErrorIncorrectTypeParameterCount BaseName Int Int
  | ErrorDuplicatedTypeParameter Identifier
  | ErrorEnumDuplicatedEntry Identifier
  | ErrorWrongType [ConcreteType] ConcreteType
  | ErrorIncompatibleType PartialType ConcreteType
  | ErrorWrongCast ConcreteType ConcreteType
  | ErrorEnumOutOfBounds BaseName Int
  | ErrorStructMissingField BaseName Identifier
  | ErrorStructDuplicatedField BaseName Identifier
  | ErrorStructUnknownField BaseName Identifier
  | ErrorStructAmbiguousType BaseName Identifier
  | ErrorStructIncompatibleTypes BaseName Identifier [ConcreteType]
  | ErrorFunctionAmbiguousType BaseName Identifier
  | ErrorFunctionIncompatibleTypes BaseName Identifier [ConcreteType]
  | ErrorFieldAccessNotAStruct ConcreteType
  | ErrorFieldAccessFieldNotFound ConcreteType Identifier
  | ErrorReservedIdentifier Identifier
  | ErrorPlaceholder Text
  | ErrorFunctionDuplicatedArg Identifier
  | ErrorBreakNotInLoop
  | ErrorContinueNotInLoop
  | ErrorFunctionCallWrongNumberOfArguments BaseName Int Int
  | ErrorDivideByZero
  | ErrorNegativeExponent
  | ErrorReferenceNotLocalVariable Resolved.Expression
  | ErrorFunctionCallArgExpectingReference Identifier
  | ErrorRValueAssignment Resolved.Expression
  | ErrorTypeParametersToTypeParameter Identifier
  | WarningNameShadow (NonEmpty Role) Identifier Role
  | WarningUnexpectedTopLevelExpression Validated.Expression
  deriving Show


newtype DiagnosisT m a = DiagnosisT (MaybeT (StateT DiagnosisState m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    )

instance MonadState s m => MonadState s (DiagnosisT m) where
  get = DiagnosisT $ lift $ lift get
  put = DiagnosisT . lift . lift . put

instance MonadTrans DiagnosisT where
  lift = DiagnosisT . lift . lift

runDiagnosisT
  :: Monad m
  => DiagnosisT m a
  -> m (Seq Diagnostic, Maybe a)
runDiagnosisT (DiagnosisT action) = do
  (maybeResult, DiagnosisState {..}) <- action
    & runMaybeT
    & flip runStateT startingState
  pure $ if _dsAnyError
         then (_dsAllDiagnostics, Nothing)
         else (_dsAllDiagnostics, maybeResult)
  where
    startingState = DiagnosisState Seq.empty False

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
  subsume       :: (Seq Diagnostic, Maybe a) -> m a

instance Monad m => MonadDiagnosis (DiagnosisT m) where
  reportWarning d = DiagnosisT do
    dsAllDiagnostics %= (:|> d)
    pure ()
  reportError d = DiagnosisT do
    dsAllDiagnostics %= (:|> d)
    dsAnyError .= True
    mzero
  try (DiagnosisT action) =
    DiagnosisT $ lift $ runMaybeT action
  ensure maybeValue = DiagnosisT do
    when (isNothing maybeValue) $
      dsAnyError .= True
    hoistMaybe maybeValue
  subsume (diagnostics, maybeResult) = do
    DiagnosisT $ dsAllDiagnostics %= (<> diagnostics)
    ensure maybeResult

instance {-# OVERLAPPABLE #-}
  ( MonadDiagnosis m
  , MonadTransControl t
  ) => MonadDiagnosis (t m) where
  reportWarning = lift . reportWarning
  reportError = lift . reportError
  ensure = lift . ensure
  try ma = liftWith (\run -> try (run ma)) >>= traverse (restoreT . pure)
  subsume = lift . subsume


tryNested :: MonadDiagnosis m => m a -> Compose m Maybe a
tryNested = Compose . try

ensureNested :: MonadDiagnosis m => Compose m Maybe a -> m a
ensureNested = getCompose >=> ensure

bracket
  :: MonadDiagnosis m
  => m a
  -> (a -> m c)
  -> (a -> m b)
  -> m b
bracket setup teardown action = do
  resource <- setup
  result <- try $ action resource
  teardown resource
  ensure result

bracket_
  :: MonadDiagnosis m
  => m a
  -> m c
  -> m b
  -> m b
bracket_ setup teardown action = do
  setup
  result <- try action
  teardown
  ensure result
