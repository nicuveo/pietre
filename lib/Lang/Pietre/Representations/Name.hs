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
  } deriving (Show, Eq, Ord, Generic)

instance Hashable BaseName

data Name = Name
  { _nameBase   :: BaseName
  , _nameParams :: [Name]
  } deriving (Show, Eq, Ord, Generic)

instance Hashable Name

makeLenses ''Name
makeLenses ''BaseName
