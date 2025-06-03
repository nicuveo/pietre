module Lang.Pietre.Stages.Analysis.Diagnostic where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens


data Diagnostic
  = ErrorImportPath   ModuleName
  | ErrorImportSymbol ModuleName Identifier
  | ErrorMultipleDeclaration Identifier (NonEmpty Location)
  | ErrorNameNotFound Path
  | ErrorNotAType Path Name
  | ErrorNotAConst Path Name
  | ErrorNotAStruct Path Name
  | ErrorAmbiguousPath Path (NonEmpty Name)
  | ErrorCyclicDefinition Name
  | ErrorIncorrectTypeParameterCount Name Int Int
  | ErrorEnumDuplicateEntries Name Identifier
  | ErrorWrongType [PathInfo Resolved] (PathInfo Resolved)
  | ErrorWrongCast (PathInfo Resolved) (PathInfo Resolved)
  | ErrorEnumOutOfBounds EnumInfo Int
  | ErrorStructMissingField (PathInfo Resolved) Identifier
  | ErrorStructDuplicatedField (PathInfo Resolved) Identifier
  | ErrorStructUnknownField (PathInfo Resolved) Identifier
  | ErrorFieldAccessNotAStruct (PathInfo Resolved)
  | ErrorFieldAccessFieldNotFound (PathInfo Resolved) Identifier
  deriving Show
