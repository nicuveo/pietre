{-# LANGUAGE OverloadedLists #-}

module Main where

import "this" Prelude                       hiding (readFile)

import Data.HashMap.Strict                  qualified as M
import Data.List.NonEmpty                   qualified as NE
import Data.Text                            qualified as T
import Data.Text.IO                         (readFile)
import System.Environment
import System.Exit
import System.FilePath

import Lang.Pietre
import Lang.Pietre.Representations.Location


help :: IO a
help = do
  putStrLn "usage:\
    \\n\
    \\n    pc [input-file-or-options]\
    \\n\
    \\noptions:\
    \\n    --help,-h                 display this help\
    \\n    -o name                   name of the output file\
    \\n    --format,-t [format]      output format (default: ppm)"
  exitFailure


renderPath :: NonEmpty Text -> String
renderPath = intercalate "::" . map T.unpack . NE.toList

main :: IO ()
main = do
  filenames <- getArgs
  symbols <-
    flip execStateT M.empty $
      for filenames \filename -> do
        dependencies <- get
        let moduleName = pure $ T.pack $ takeBaseName filename
        source  <- liftIO $ readFile filename
        ast     <- parseModule filename source `onLeft` (error . show)
        let (diagnostics, result) = analyzeModule dependencies moduleName ast
        when (not $ null diagnostics) $
          liftIO $ print (moduleName, diagnostics)
        case result of
          Just symbols -> modify $ M.insert moduleName symbols
          Nothing      -> error "aborting"
  for_ (M.toList symbols) \(moduleName, decls) -> do
    putStrLn $ "module " ++ renderPath moduleName
    for_ (M.toList decls) \(declName, WithLocation _ decl) -> do
      putStrLn $ "  " ++ T.unpack declName ++ ": " ++ show decl
    putStrLn ""
