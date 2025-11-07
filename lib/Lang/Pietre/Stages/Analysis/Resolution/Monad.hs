module Lang.Pietre.Stages.Analysis.Resolving.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra                    (whenM)
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                    qualified as M
import Data.Set                               qualified as S
import Data.Tuple

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Diagnostic


--------------------------------------------------------------------------------
-- Monad

type Resolve m = ReaderT ResolveInfo (StateT ResolveContext m)


data ResolveInfo = ResolveInfo
  { _riName     :: BaseName
  , _riLocation :: Location
  }

data ResolveContext = ResolveContext
  { _rcScope    :: Scope
  , _rcLocation :: Location
  }


runResolve
  :: BaseName
  -> Scope
  -> Location
  -> Resolve m a
runResolve name scope loc action = action
  & flip runReaderT (ResolveInfo name loc)
  & flip execStateT (ResolveContext scope loc)


makeLenses ''ResolveInfo
makeLenses ''ResolveContext


lookupName
  :: Monad m
  => Path
  -> Resolve m (Maybe (NonEmpty [Role]))
lookupName = uses rcScope . M.lookup
