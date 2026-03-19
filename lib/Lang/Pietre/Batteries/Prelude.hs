{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Batteries.Prelude where

import "this" Prelude

import Data.HashMap.Strict                   qualified as M
import Data.Text.IO                          qualified as T
import Language.Haskell.TH                   as TH
import Language.Haskell.TH.Syntax            as TH

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Stages.Analysis
import Lang.Pietre.Stages.Parsing


preludeInterface :: Interface
preludeInterface = $(
  do
    filePath <- makeRelativeToProject "lib/Lang/Pietre/Batteries/Prelude.pi"
    source   <- runIO $ T.readFile filePath
    addDependentFile filePath
    let (diagnostics, result) = runDiagnosis do
          preludeModule <- parseModule filePath source
          analyzeModule
            M.empty
            M.empty
            M.empty
            M.empty
            (pure "Prelude")
            preludeModule
    for_ diagnostics \d ->
      TH.reportError $ show d
    case result of
      Nothing        -> fail "could not compile Prelude"
      Just interface -> [| interface |]
  )
