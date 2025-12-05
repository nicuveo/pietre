module Main where

import "this" Prelude                                  hiding (readFile)

import Data.HashMap.Strict.Extra                       qualified as M
import Data.List.NonEmpty                              qualified as NE
import Data.Text                                       qualified as T
import Data.Text.IO                                    qualified as T
import Lucid
import System.Environment
import System.Exit
import System.FilePath

import Lang.Pietre
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
-- import Lang.Pietre.Representations.IR         as IR
import Lang.Pietre.Export.HTML
import Lang.Pietre.Export.PrettyPrinting.AST.Parsed    as PPP
import Lang.Pietre.Export.PrettyPrinting.AST.Validated as VPP
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

renderBaseName :: BaseName -> Text
renderBaseName BaseName {..} =
  T.intercalate "::" (map rawIdentifier $ NE.toList _nameModule) <> "::" <> rawIdentifier _nameIdent

renderName :: Name -> Text
renderName Name {..} =
  case _nameParams of
    [] -> renderBaseName _nameBase
    ps -> renderBaseName _nameBase <> "<" <> T.intercalate "," (map renderName ps) <> ">"

main :: IO ()
main = do
  filenames <- getArgs
  (diagnostics, result) <-
    runDiagnosisT $
      flip execStateT (M.empty, M.empty, M.empty, M.empty) $
        for filenames \filename -> do
          (interfaces, definitions, symbols, functions) <- get
          let moduleName = pure $ Identifier $ T.pack $ takeBaseName filename
          source    <- liftIO $ T.readFile filename
          parsedAST <- parseModule filename source `onLeft` (error . show)
          interface@Interface {..} <- simplifyModule <$>
            analyzeModule
              interfaces
              definitions
              functions
              symbols
              moduleName
              parsedAST
          let debugFilename = takeBaseName filename ++ "-ast.html"
          liftIO $ T.writeFile debugFilename $ renderHTML do
            h2_ "Parsed AST"
            PPP.prettyPrintHTML parsedAST
            h2_ "Validated AST"
            VPP.prettyPrintHTML interface
          put ( M.insert moduleName interface interfaces
              , definitions <> _interfaceDefinitions
              , symbols     <> _interfaceSymbols
              , functions   <> _interfaceFunctions
              )
  traverse_ print diagnostics
  case result of
    Nothing -> error "aborting"
    Just (_interfaces, definitions, symbols, functions) -> do
      putStrLn "################################################################################"
      putStrLn "## Definitions"
      M.forWithKey_ definitions \baseName def -> do
        putStrLn $ T.unpack $ renderBaseName baseName
        print def
      putStrLn "################################################################################"
      putStrLn "## Functions"
      M.forWithKey_ functions \baseName def -> do
        putStrLn $ T.unpack $ renderBaseName baseName
        print $ _located def
      putStrLn "################################################################################"
      putStrLn "## Symbols"
      M.forWithKey_ symbols \name def -> do
        putStrLn $ T.unpack $ renderName name
        print def

{-
printFunction :: Name -> IR.Function -> IO ()
printFunction name IR.Function {..} = do
  putStrLn $ "function " ++ T.unpack (renderName name) ++ " {"
  traverse_ (uncurry printBlock) _funBlocks
  putStrLn "}"

printBlock :: IR.Label -> IR.Block -> IO ()
printBlock label IR.Block {..} = do
  putStrLn $ "  " ++ show label ++ " " ++ show _blockArguments ++ ":"
  for_ _blockInstructions \inst ->
    putStrLn $ "    " ++ show inst
  putStrLn $ "    " ++ show _blockTerminator
-}
