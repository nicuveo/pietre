{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Assembly.Monad where

import "this" Prelude

import Control.Lens
import Data.Monoid

import Lang.Pietre.Stages.Assembly.Color
import Lang.Pietre.Stages.Assembly.Templates


type Assemble = State AssemblyState

data AssemblyState = AssemblyState
  { _asCurrentColor :: Color
  , _asCurrentImage :: Image
  , _asLastEntrance :: Last Int
  }

makeLenses 'AssemblyState

runAssembly :: Color -> Assemble a -> a
runAssembly color = flip evalState $ AssemblyState color (functionTemplate color) mempty
