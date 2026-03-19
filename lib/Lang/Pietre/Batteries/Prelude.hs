{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Batteries.Prelude where

import "this" Prelude

import Data.HashMap.Strict                   qualified as M
import Data.Text.IO                          qualified as T
import Language.Haskell.TH.Syntax            as TH hiding (Name)

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.IR        (Label (..))
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis
import Lang.Pietre.Stages.Parsing


preludeModuleName :: ModuleName
preludeModuleName = pure "Prelude"

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

preludeObject :: Object
preludeObject =
  [ (readIntName,  readIntBytecode)
  , (writeIntName, writeIntBytecode)
  ]
  where
    readIntName  = Name (BaseName ["Prelude"] "read_int")  []
    writeIntName = Name (BaseName ["Prelude"] "write_int") []
    readIntBytecode =
      [ Entrance (readIntName, Label 0 0)
      , InInt -- TODO: what do we do if this fails?!
      , PushInt 2
      , PushInt 1
      , Roll
      , Return
      ]
    writeIntBytecode =
      [ Entrance (writeIntName, Label 0 0)
      , OutInt
      , Return
      ]
