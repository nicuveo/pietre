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
  (_, definitions, _, _) <-
    flip execStateT (M.empty, M.empty, M.empty, M.empty) $
      for filenames \filename -> do
        (exports, definitions, symbols, functions) <- get
        let moduleName = pure $ T.pack $ takeBaseName filename
        source    <- liftIO $ readFile filename
        parsedAST <- parseModule filename source `onLeft` (error . show)
        let (diagnostics, resolvedAST) = analyzeModule
              definitions
              symbols
              functions
              exports
              moduleName
              parsedAST
        unless (null diagnostics) $
          liftIO $ print (moduleName, diagnostics)
        case simplifyModule <$> resolvedAST of
          Nothing -> error "aborting"
          Just (ResolvedModule {..}) -> do
            put ( M.insert moduleName _resmodExported exports
                , definitions <> _resmodDefinitions
                , symbols     <> _resmodSymbols
                , functions   <> _resmodFunctions
                )
  for_ (M.toList definitions) \(name, WithLocation _ decl) -> do
    let declName = T.intercalate "::" $ NE.toList $ _nameFullPath name
    putStrLn $ T.unpack declName ++ ": " ++ show decl
