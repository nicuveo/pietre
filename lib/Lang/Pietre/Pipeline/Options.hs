{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Pipeline.Options where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE


data CompilerCommand
  = Compile CompilerOptions CompilerFlags FilePath
  | Help
  deriving Show

data CompilerOptions = CompilerOptions
  { _coVerbose      :: Bool
  , _coIncludePaths :: Seq FilePath
  } deriving Show

data CompilerFlags = CompilerFlags
  { _cfSimplify :: Bool
  , _cfOptimize :: Bool
  , _cfMinimize :: Bool
  } deriving Show

makeLenses ''CompilerOptions
makeLenses ''CompilerFlags


defaultOptions :: CompilerOptions
defaultOptions = CompilerOptions
  { _coVerbose      = False
  , _coIncludePaths = pure "."
  }

defaultFlags :: CompilerFlags
defaultFlags = CompilerFlags
  { _cfSimplify = True
  , _cfOptimize = False
  , _cfMinimize = True
  }


parseCommand
  :: MonadDiagnosis m
  => [String]
  -> m CompilerCommand
parseCommand args = do
  -- TODO
  case args of
    [fileName] -> pure $ Compile defaultOptions defaultFlags fileName
    []         -> reportError $ Diagnostic Nothing Nothing ErrorNoMainProvided
    _          -> unimplemented
