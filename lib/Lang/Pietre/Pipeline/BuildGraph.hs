module Lang.Pietre.Pipeline.BuildGraph where

import "this" Prelude

import Data.HashMap.Strict                    qualified as M
import Data.List                              qualified as L
import Data.Sequence                          qualified as Seq
import Data.Text                              qualified as T
import System.FilePath

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Bytecode   (Object)
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface  (Interface)
import Lang.Pietre.Representations.Location
import Lang.Pietre.Stages.Parsing


data BuildGraph = BuildGraph
  { _bgRoot :: Node
  , _bfInfo :: HashMap FilePath NodeInfo
  }

data Node = Node
  { _nFile :: FilePath
  , _nDeps :: Seq Node
  }

data NodeInfo
  = Source    Module
  | Interface Module Interface
  | Object    Module Interface Object

class Monad m => MonadBuild m where
  findSourceFile        :: FilePath -> m (Maybe FilePath)
  readSourceFile        :: FilePath -> m Text
  lookupCachedInterface :: FilePath -> m (Maybe Interface)
  lookupCachedObject    :: FilePath -> m (Maybe Object)

constructBuildGraph
  :: forall m
   . (MonadBuild m, MonadDiagnosis m)
  => FilePath
  -> m BuildGraph
constructBuildGraph mainFile = do
  (rootNode, nodeMap) <-
    flip runStateT M.empty $
    buildNode [] impossibleLocation mainFile
  pure $ BuildGraph rootNode $ fmap snd nodeMap
  where
    impossibleLocation = reportICE
      "constructBuildGraph"
      "circular import without import?"
      ["file: " ++ mainFile]
    buildNode :: [FilePath] -> Location -> FilePath -> StateT (HashMap FilePath (Node, NodeInfo)) m Node
    buildNode parents importLocation sourcePath = do
      when (sourcePath `L.elem` parents) $
        reportError $ Diagnostic
          { _diagnosticDeclaration = Nothing
          , _diagnosticLocation    = importLocation
          , _diagnosticMessage     = ErrorCircularImport sourcePath (L.reverse parents)
          }
      gets (M.lookup sourcePath) >>= \case
        Just (node, _) -> pure node
        Nothing -> do
          sourceCode <- lift $ readSourceFile sourcePath
          nodeModule <- parseModule sourcePath sourceCode
          nodeInfo   <- lift (lookupCachedInterface sourcePath) >>= \case
            Nothing -> pure $ Source nodeModule
            Just i  -> lift (lookupCachedObject sourcePath) <&> \case
              Nothing -> Interface nodeModule i
              Just o  -> Object nodeModule i o
          dependencies <- for (_modImports nodeModule) \(WithLocation depImportLocation depImportStatement) -> do
            let depRelSourcePath = mkFilePath depImportStatement
            depSourcePath <- lift (findSourceFile depRelSourcePath) `onNothingM`
              reportError Diagnostic
                { _diagnosticDeclaration = Nothing
                , _diagnosticLocation    = depImportLocation
                , _diagnosticMessage     = ErrorFileNotFound depRelSourcePath
                }
            buildNode (sourcePath : parents) depImportLocation depSourcePath
          let node = Node sourcePath $ Seq.fromList dependencies
          modify $ M.insert sourcePath (node, nodeInfo)
          pure node
    mkFilePath (Import pathComponents _) =
      joinPath $ toList $ fmap (T.unpack . rawIdentifier) pathComponents
