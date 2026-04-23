{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Name where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Representations.Identifier


type Path = NonEmpty Identifier

type ModuleName = NonEmpty Identifier

data BaseName = BaseName
  { _nameModule :: ModuleName
  , _nameIdent  :: Identifier
  } deriving (Eq, Ord, Generic, Lift)

instance Show BaseName where
  show (BaseName moduleName identifier) = concat
    [ "BaseName "
    , show (toList moduleName)
    , " "
    , show identifier
    ]

instance Hashable BaseName

data Name = Name
  { _nameBase   :: BaseName
  , _nameParams :: [Name]
  } deriving (Eq, Ord, Generic, Lift)

instance Show Name where
  show (Name baseName params) = concat
    [ "Name ("
    , show baseName
    , ") "
    , show params
    ]

instance Hashable Name

makeLenses ''Name
makeLenses ''BaseName
