module Locate where

import "this" Prelude

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath


listFiles :: FilePath -> String -> Q Exp
listFiles dir extension = do
  files <- qRunIO (listDirectory dir) >>= traverse \name -> do
    let path = dir </> name
    isDir <- qRunIO $ doesDirectoryExist path
    if isDir || takeExtension name /= extension
    then pure Nothing
    else do
      addDependentFile path
      pure $ Just $ LitE $ StringL path
  pure $ ListE $ catMaybes files
