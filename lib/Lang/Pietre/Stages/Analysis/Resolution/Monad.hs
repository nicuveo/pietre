{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Analysis.Resolution.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                      qualified as M

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Resolved
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


type Resolve = DiagnosisT (ReaderT ResolveInfo (State ResolveContext))

type Scope = HashMap Path (NonEmpty Role)

data ResolveInfo = ResolveInfo
  { _riDeclarationName :: BaseName
  }

data ResolveContext = ResolveContext
  { _rcScope    :: Scope
  , _rcLocation :: Location
  }

runResolve
  :: MonadDiagnosis m
  => BaseName
  -> Scope
  -> Location
  -> Resolve a
  -> m a
runResolve declarationName topLevelScope declarationLocation action =
  action
    & runDiagnosisT
    & flip runReaderT (ResolveInfo declarationName)
    & flip evalState (ResolveContext topLevelScope declarationLocation)
    & subsume


makeLenses ''ResolveInfo
makeLenses ''ResolveContext


lookupName :: Path -> Resolve (Maybe (NonEmpty Role))
lookupName = uses rcScope . M.lookup


fatal :: Message -> Resolve a
fatal message = do
  declName <- view riDeclarationName
  declLocation <- use rcLocation
  reportError $ Diagnostic (Just declName) declLocation message

warn :: Message -> Resolve ()
warn message = do
  declName <- view riDeclarationName
  declLocation <- use rcLocation
  reportWarning $ Diagnostic (Just declName) declLocation message
