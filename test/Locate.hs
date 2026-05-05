module Locate
  ( listFiles
  , listFolders
  ) where

import "this" Prelude

import Control.Monad.Extra        (ifM)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath


listFiles :: FilePath -> String -> Q Exp
listFiles dir extension = do
  files <- listContentWith isMatchingFile dir >>= traverse \path -> do
    addDependentFile path
    pure $ LitE $ StringL path
  pure $ ListE files
  where
    isMatchingFile path = do
      isDir <- doesDirectoryExist path
      pure $ not isDir && takeExtension path == extension

listFolders :: FilePath -> Q Exp
listFolders dir = do
  files <- listContentWith isFolder dir >>= traverse \folderPath -> do
    listContentWith isFile folderPath >>= traverse \filePath -> do
      addDependentFile filePath
    pure $ LitE $ StringL folderPath
  pure $ ListE files
  where
    isFolder = doesDirectoryExist
    isFile   = fmap not . doesDirectoryExist


listContentWith :: (FilePath -> IO Bool) -> FilePath -> Q [FilePath]
listContentWith predicate dir = qRunIO
  $ fmap catMaybes
  $ listDirectory dir >>= traverse go
  where
    go name = do
      let path = dir </> name
      ifM (predicate path) (pure $ Just path) (pure Nothing)
