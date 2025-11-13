{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Name where

import "this" Prelude

import Control.Lens
import Data.List.NonEmpty                     qualified as NE

import Lang.Pietre.Representations.Identifier


type Path = NonEmpty Identifier

type ModuleName = NonEmpty Identifier

data BaseName = BaseName
  { _nameModule :: ModuleName
  , _nameIdent  :: Identifier
  }

data Name = Name
  { _nameBase   :: BaseName
  , _nameParams :: [Name]
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Name

pattern ConcreteName :: BaseName -> Name
pattern ConcreteName baseName = NameTree baseName []

makeLenses ''Name
makeLenses ''BaseName
