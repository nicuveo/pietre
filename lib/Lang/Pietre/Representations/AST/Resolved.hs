{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Pietre.Representations.AST where

import                "this" Prelude

import                Control.Lens
import                Data.Kind
import                Data.List.NonEmpty                     qualified as NE
import                Prettyprinter
import                Prettyprinter.Render.Text

import                Lang.Pietre.Representations.Identifier
import                Lang.Pietre.Representations.Location
import {-# SOURCE #-} Lang.Pietre.Representations.Name


data Resolved

instance ASTRepresentation Resolved where
  type NameType Resolved = Role


data Role
  = BuiltinType BaseName
  | BuiltinFunction BaseName
  | Struct BaseName
  | Enum BaseName
  | Constant BaseName
  | Function BaseName
  | TypeAlias BaseName
  | TypeParameter BaseName Identifier
  | Placeholder
  | FunctionPointer (FunctionType Resolved)
  | FunctionArgument Identifier (FunctionArgType Resolved)
  | LetVariable Identifier (Maybe (PathInfo Resolved))
  deriving (Show, Eq, Ord, Generic)

instance Hashable Role


enumRoleFromConstructorRole :: Identifier -> Role -> Role
enumRoleFromConstructorRole identifier = \case
  Enum Name {..} ->
    Constant $ Name (NE.fromList $ NE.init _nameFullPath ++ [identifier]) _nameParameters
  role -> reportICE
    "enum typename resolution"
    "enum constructor role isn't a top level declaration"
    [ "enum type name:   " ++ show identifier
    , "constructor role: " ++ show role
    ]
