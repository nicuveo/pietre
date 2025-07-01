module Lang.Pietre.Stages.Analysis.Diagnostic where

import "this" Prelude

import Control.Monad.Writer.Class

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens

{-
data Diagnostic = Diagnostic
  { _diagnosticModule      :: ModuleName
  , _diganosticDeclaration :: Identifier
  , _diagnosticLocation    :: Location
  , _diagnosticInfo        :: DiagnosticInfo
  }
-}

data Diagnostic
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
  | ErrorDuplicateTypeParameter Name Identifier
  | ErrorEnumDuplicateEntries Name Identifier
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
  | ErrorReservedIdentifier Name Identifier
  | ErrorPlaceholder Text
  | ErrorFunctionDuplicatedArg Identifier
  | ErrorBreakNotInLoop
  | ErrorContinueNotInLoop
  | ErrorFunctionCallWrongNumberOfArguments (PathInfo Resolved) Int Int
  | WarningNameShadow (NonEmpty Role) Role
  deriving Show

isError :: Diagnostic -> Bool
isError = \case
  WarningNameShadow _ _ -> False
  _ -> True


class Monad m => MonadDiagnostic m where
  report :: Diagnostic -> m ()
  abort  :: m a

  fatal  :: Diagnostic -> m a
  fatal d = report d >> abort

instance MonadDiagnostic (MaybeT ((,) [Diagnostic])) where
  report = tell . pure
  abort = mzero
