module Lang.Pietre.Representations.Name where

import "this" Prelude

import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Tokens


type ModuleName = NonEmpty Identifier

data Name = Name
  { _nameFullPath   :: NonEmpty Identifier
  , _nameParameters :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Name

data Role
  = TopLevelDeclaration Name
  | BuiltinType Identifier
  | BuiltinFunction Name
  | TypeParameter Name Identifier
  | Placeholder
  | FunctionPointer (FunctionType Resolved)
  | FunctionArgument Identifier (FunctionArgType Resolved)
  | LetVariable Identifier (PathInfo Resolved)
  deriving (Show)
