{-# LANGUAGE OverloadedLists #-}

module Lang.Pietre.Pipeline.Compile (compileBinary) where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra                             (whenJustM)
import Data.HashMap.Strict                             qualified as M
import Data.List                                       qualified as L
import Data.List.NonEmpty                              qualified as NE
import Data.Sequence                                   qualified as Seq
import Data.Text                                       qualified as T
import Data.Tuple.Extra
import System.FilePath

import Lang.Pietre.Batteries.Prelude
import Lang.Pietre.Export.Bytecode
import Lang.Pietre.Export.HTML
import Lang.Pietre.Export.IR.Dot
import Lang.Pietre.Export.PrettyPrinting.AST.Validated
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Pipeline.Monad
import Lang.Pietre.Pipeline.Options
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Binary              (Binary)
import Lang.Pietre.Representations.Bytecode            (Object)
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Image
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.IR
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis
import Lang.Pietre.Stages.Assembly
import Lang.Pietre.Stages.Generation
import Lang.Pietre.Stages.Linking
import Lang.Pietre.Stages.Lowering
import Lang.Pietre.Stages.Minimization
import Lang.Pietre.Stages.Parsing
import Lang.Pietre.Stages.Simplification


compileBinary
  :: (MonadFileSystem m, MonadDiagnosis m)
  => CompilerOptions
  -> CompilerFlags
  -> FilePath
  -> m Image
compileBinary compilerOptions moduleFlags mainFile =
  runCompile compilerOptions moduleFlags do
    let
      mainModuleName = pure "Main"
      mainSymbolName = Name (BaseName mainModuleName "main") []
    buildPlan <- createBuildPlan mainModuleName mainFile
    traverse_ (uncurry3 compileModule) buildPlan
    allObjects <- M.unions . M.elems <$> use ccObjects
    binary <- link mainSymbolName allObjects -- <> Prelude.objects
    whenJustM (view $ ciCompilerOptions . coExportBinary) $
      exportBinary binary
    pure $ assemble binary

compileModule
  :: (MonadFileSystem m, MonadDiagnosis m)
  => ModuleName
  -> FilePath
  -> Module
  -> Compile m ()
compileModule moduleName sourceFile moduleInfo = do
  -- log: [1/20] Compiling moduleName
  CompileContext {..} <- get

  -- analysis
  shouldSimplify <- view $ ciModuleFlags . cfSimplify
  interface <-
    maybeApply shouldSimplify simplifyModule <$>
    analyzeModule
      _ccInterfaces
      _ccDefinitionCache
      _ccFunctionCache
      _ccSymbolCache
      moduleName
      (addPrelude sourceFile moduleInfo)
  addInterface moduleName interface
  whenJustM (view $ ciCompilerOptions . coExportAST) $
    exportAST moduleName interface
  -- add to interface cache

  -- lowering
  -- shouldOptimize <- view $ ciModuleFlags . cfOptimize
  moduleIR <- lowerModule interface
  whenJustM (view $ ciCompilerOptions . coExportIR) $
    exportIR moduleName moduleIR

  -- code generation
  shouldMinimize <- view $ ciModuleFlags . cfMinimize
  let object =
        maybeApply shouldMinimize (fmap minimize) $
        M.mapWithKey generateBytecode moduleIR
  ccObjects %= M.insert moduleName object
  whenJustM (view $ ciCompilerOptions . coExportBytecode) $
    exportBytecode moduleName object
  -- add to object cache

addPrelude
  :: FilePath
  -> Module
  -> Module
addPrelude sourceFile =
  modImports <>:~ [WithLocation (initialLocation sourceFile) (Import preludeModuleName Exhaustive)]

exportAST
  :: (MonadFileSystem m)
  => ModuleName
  -> Interface
  -> FilePath
  -> Compile m ()
exportAST moduleName interface folder = do
  writeToFile (generateDebugPath moduleName folder ".html") (renderHTML $ prettyPrintHTML interface)

exportIR
  :: (MonadFileSystem m)
  => ModuleName
  -> IR
  -> FilePath
  -> Compile m ()
exportIR moduleName ir folder = do
  writeToFile (generateDebugPath moduleName folder ".dot") (renderIR ir)

exportBytecode
  :: (MonadFileSystem m)
  => ModuleName
  -> Object
  -> FilePath
  -> Compile m ()
exportBytecode moduleName object folder = do
  writeToFile (generateDebugPath moduleName folder ".txt") (renderBytecode object)

exportBinary
  :: (MonadFileSystem m)
  => Binary
  -> FilePath
  -> Compile m ()
exportBinary binary folder = do
  writeToFile (generateDebugPath (pure "Main") folder ".linked.txt") $ renderBinary binary

generateDebugPath
  :: ModuleName
  -> FilePath
  -> String
  -> FilePath
generateDebugPath moduleName folder ext =
  folder </> L.intercalate "_" (map (T.unpack . rawIdentifier) (NE.toList moduleName)) ++ ext

createBuildPlan
  :: forall m
   . (MonadFileSystem m, MonadDiagnosis m)
  => ModuleName
  -> FilePath
  -> Compile m (Seq (ModuleName, FilePath, Module))
createBuildPlan mainName mainPath = go Seq.empty Nothing mainName mainPath
  where
    go parents importLocation moduleName sourcePath = do
      let
        throwDiagnostic :: Message -> Compile m a
        throwDiagnostic = reportError . Diagnostic Nothing importLocation
      when (moduleName `L.elem` parents) $
        throwDiagnostic $ ErrorCircularImport moduleName parents
      uses ccModules (M.lookup moduleName) >>= \case
        Just _ ->
          pure Seq.empty
        Nothing -> do
          sourceCode <- readSourceFile sourcePath `onNothingM`
            throwDiagnostic (ErrorFileNotFound sourcePath)
          parsedModule <- parseModule sourcePath sourceCode
          ccModules %= M.insert moduleName parsedModule
          buildPlan <- for (_modImports parsedModule) \(WithLocation depLocation depImport) -> do
            let depName = _importPath depImport
            depPath <- locateSourceFile depLocation depName
            go (parents |> moduleName) (Just depLocation) depName depPath
          pure $ mconcat buildPlan |> (moduleName, sourcePath, parsedModule)

locateSourceFile
  :: (MonadFileSystem m, MonadDiagnosis m)
  => Location
  -> ModuleName
  -> Compile m FilePath
locateSourceFile importLocation moduleName = do
  includePaths <- view $ ciCompilerOptions . coIncludePaths
  traverse go includePaths >>= \allPaths -> case fold allPaths of
    [filePath] ->
      pure filePath
    [] ->
      throwDiagnostic $ ErrorModuleNotFound moduleName
    filePaths ->
      throwDiagnostic $ ErrorAmbiguousModule moduleName filePaths
  where
    throwDiagnostic = reportError . Diagnostic Nothing (Just importLocation)
    relativeFilePath = joinPath $ toList $ fmap (T.unpack . rawIdentifier) moduleName
    go folder = do
      let targetFilePath = folder </> relativeFilePath
      isFound <- doesFileExist targetFilePath
      pure $ Seq.fromList [targetFilePath | isFound]


maybeApply
  :: Bool
  -> (a -> a)
  -> a
  -> a
maybeApply True  f x = f x
maybeApply False _ x = x
