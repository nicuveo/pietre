{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Resolution.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                      qualified as M

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Resolved
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


--------------------------------------------------------------------------------
-- Monad

type ResolveT m = ReaderT ResolveInfo (StateT ResolveContext m)

type Scope = HashMap Path (NonEmpty Role)

data ResolveInfo = ResolveInfo
  { _riDeclarationName :: BaseName
  }

data ResolveContext = ResolveContext
  { _rcScope    :: Scope
  , _rcLocation :: Location
  }

runResolveT
  :: Monad m
  => BaseName
  -> Scope
  -> Location
  -> ResolveT m a
  -> m a
runResolveT declarationName topLevelScope declarationLocation action =
  action
    & flip runReaderT (ResolveInfo declarationName)
    & flip evalStateT (ResolveContext topLevelScope declarationLocation)


makeLenses ''ResolveInfo
makeLenses ''ResolveContext


lookupName
  :: Monad m
  => Path
  -> ResolveT m (Maybe (NonEmpty Role))
lookupName = uses rcScope . M.lookup


fatal :: MonadDiagnosis m => Message -> ResolveT m a
fatal message = do
  declName <- view riDeclarationName
  declLocation <- use rcLocation
  reportError $ Diagnostic (Just declName) declLocation message

warn :: MonadDiagnosis m => Message -> ResolveT m ()
warn message = do
  declName <- view riDeclarationName
  declLocation <- use rcLocation
  reportWarning $ Diagnostic (Just declName) declLocation message
