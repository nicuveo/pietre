module Lang.Pietre.Stages.Analysis.Diagnostic where

import "this" Prelude

import Control.Monad.Writer.Class

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens

data Diagnostic = Diagnostic
  { _diganosticDeclaration :: Maybe Name
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
  | ErrorDuplicateTypeParameter Identifier
  | ErrorEnumDuplicateEntries Identifier
  | ErrorWrongType [PathInfo Resolved] (PathInfo Resolved)
  | ErrorWrongCast (PathInfo Resolved) (PathInfo Resolved)
  | ErrorEnumOutOfBounds (EnumInfo Resolved) Int
  | ErrorStructMissingField (PathInfo Resolved) Identifier
  | ErrorStructDuplicatedField (PathInfo Resolved) Identifier
  | ErrorStructUnknownField (PathInfo Resolved) Identifier
  | ErrorStructAmbiguousType (PathInfo Resolved) Identifier
  | ErrorStructIncompatibleTypes (PathInfo Resolved) Identifier (NonEmpty (PathInfo Resolved))
  | ErrorFunctionAmbiguousType (PathInfo Resolved) Identifier
  | ErrorFunctionIncompatibleTypes (PathInfo Resolved) Identifier (NonEmpty (PathInfo Resolved))
  | ErrorFieldAccessNotAStruct (PathInfo Resolved)
  | ErrorFieldAccessFieldNotFound (PathInfo Resolved) Identifier
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
  | ErrorFunctionCallArgExpectingValue     Identifier
  | ErrorRValueAssignment (Expression Resolved)
  | ErrorIfExprNotBoolean (PathInfo Resolved)
  | ErrorWhileExprNotBoolean (PathInfo Resolved)
  | ErrorTypeParametersToTypeParameter Identifier
  | WarningNameShadow (NonEmpty Role) Role
  | WarningUnexpectedTopLevelExpression (Expression Resolved)
  deriving Show

isErrorMsg :: Message -> Bool
isErrorMsg = \case
  WarningNameShadow _ _                 -> False
  WarningUnexpectedTopLevelExpression _ -> False
  _                                     -> True

isError :: Diagnostic -> Bool
isError = isErrorMsg . _diagnosticMessage


class Monad m => MonadDiagnostic m where
  report :: Message -> m ()
  abort  :: m a

  fatal  :: Message -> m a
  fatal d = report d >> abort

instance MonadDiagnostic (MaybeT ((,) [Diagnostic])) where
  report msg = tell $ pure $ Diagnostic Nothing (initialLocation "") msg
  abort = mzero
