module Lang.Pietre.Representations.Name where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Tokens


type ModuleName = NonEmpty Identifier

data Name
  = TopLevelDeclaration ModuleName Identifier
  | BuiltinType Identifier
  | BuiltinFunction Identifier
  | TypeParameter Identifier
  | Placeholder
  | FunctionArgument Identifier (FunctionArgType Resolved)
  | LetVariable Identifier (PathInfo Resolved)
  deriving (Show, Eq, Ord, Generic)

instance Hashable Name
