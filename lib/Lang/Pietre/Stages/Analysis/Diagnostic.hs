module Lang.Pietre.Stages.Analysis.Diagnostic where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


data Diagnostic
  = ErrorImportPath ModuleName
  | ErrorImportSymbol ModuleName Identifier
  | ErrorMultipleDeclaration Identifier (NonEmpty Location)
  | ErrorRoleNotFound Path
  | ErrorNotAType Path Role
  | ErrorNotAConst Path Role
  | ErrorNotAStruct Path Role
  | ErrorNotAValue Path Role
  | ErrorNotAFunction Path Role
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
  | ErrorFieldAccessNotAStruct (PathInfo Resolved)
  | ErrorFieldAccessFieldNotFound (PathInfo Resolved) Identifier
  | ErrorReservedIdentifier Name Identifier
  | ErrorPlaceholder Text
  | ErrorFunctionDuplicatedArg Identifier
  | ErrorBreakNotInLoop
  | ErrorContinueNotInLoop
  | WarningNameShadow (NonEmpty Role) Role
  deriving Show
