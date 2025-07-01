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
import Lang.Pietre.Representations.Name


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
  (definitions, _) <-
    flip execStateT (M.empty, M.empty) $
      for filenames \filename -> do
        (defCache, exports) <- get
        let moduleName = pure $ T.pack $ takeBaseName filename
        source <- liftIO $ readFile filename
        ast    <- parseModule filename source `onLeft` (error . show)
        let (diagnostics, result) = analyzeModule defCache exports moduleName ast
        unless (null diagnostics) $
          liftIO $ print (moduleName, diagnostics)
        case result of
          Nothing -> error "aborting"
          Just (ResolvedModule exported newDefinitions) -> do
            put ( defCache <> newDefinitions
                , M.insert moduleName exported exports
                )
  for_ (M.toList definitions) \(name, WithLocation _ decl) -> do
    let declName = T.intercalate "::" $ NE.toList $ _nameFullPath name
    putStrLn $ T.unpack declName ++ ": " ++ show decl
