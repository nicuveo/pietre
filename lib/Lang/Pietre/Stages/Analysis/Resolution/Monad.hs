module Lang.Pietre.Stages.Analysis.Resolving.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra                      (whenM)
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                      qualified as M
import Data.Set                                 qualified as S
import Data.Tuple

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Diagnostic


--------------------------------------------------------------------------------
-- Monad

type ResolveT m = ReaderT ResolveInfo (StateT ResolveContext m)

type Scope = HashMap Path (NonEmpty Role)

data ResolveInfo = ResolveInfo
  { _riModuleName      :: ModuleName
  , _riDeclarationName :: BaseName
  }

data ResolveContext = ResolveContext
  { _rcScope    :: Scope
  , _rcLocation :: Location
  }

runResolveT
  :: ModuleName
  -> BaseName
  -> Scope
  -> Location
  -> ResolveT m a
runResolveT moduleName declarationName topLevelScope declarationLocation action =
  action
    & flip runReaderT (ResolveInfo moduleName declarationName)
    & flip execStateT (ResolveContext topLevelScope declarationLocation)


makeLenses ''ResolveInfo
makeLenses ''ResolveContext


lookupName
  :: Monad m
  => Path
  -> ResolveT m (Maybe (NonEmpty [Role]))
lookupName = uses rcScope . M.lookup
