{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Name where

import "this" Prelude

import Control.Lens
import Data.List.NonEmpty                     qualified as NE

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Identifier


type ModuleName = NonEmpty Identifier

data Name = Name
  { _nameFullPath   :: NonEmpty Identifier
  , _nameParameters :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Name

data Role
  = TopLevelDeclaration Name
  | BuiltinType Name
  | BuiltinFunction Name
  | TypeParameter Name Identifier
  | Placeholder
  | FunctionPointer (FunctionType Resolved)
  | FunctionArgument Identifier (FunctionArgType Resolved)
  | LetVariable Identifier (PathInfo Resolved)
  deriving (Show, Eq, Ord, Generic)

instance Hashable Role


makeLenses ''Name


enumRoleFromConstructorRole :: Identifier -> Role -> Role
enumRoleFromConstructorRole identifier = \case
  TopLevelDeclaration Name {..} ->
    TopLevelDeclaration $ Name (NE.fromList $ NE.init _nameFullPath ++ [identifier]) _nameParameters
  role -> reportICE
    "enum typename resolution"
    "enum constructor role isn't a top level declaration"
    [ "enum type name:   " ++ show identifier
    , "constructor role: " ++ show role
    ]
