{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Pipeline.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                    qualified as M

import Lang.Pietre.Pipeline.Options
import Lang.Pietre.Representations.AST.Parsed
import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name


type Compile m = ReaderT CompileInfo (StateT CompileContext m)

data CompileInfo = CompileInfo
  { _ciCompilerOptions :: CompilerOptions
  , _ciModuleFlags     :: CompilerFlags
  }

data CompileContext = CompileContext
  { _ccModules         :: HashMap ModuleName Module
  , _ccInterfaces      :: HashMap ModuleName Interface
  , _ccObjects         :: HashMap ModuleName Object
  , _ccDefinitionCache :: DefinitionCache
  , _ccFunctionCache   :: FunctionCache
  , _ccSymbolCache     :: SymbolCache
  }

makeLenses ''CompileInfo
makeLenses ''CompileContext


class Monad m => MonadFileSystem m where
  doesFileExist  :: FilePath -> m Bool
  readSourceFile :: FilePath -> m (Maybe Text)
  -- lookupCachedInterface :: FilePath -> m (Maybe Interface)
  -- lookupCachedObject    :: FilePath -> m (Maybe Object)

instance (MonadTrans t, MonadFileSystem m) => MonadFileSystem (t m) where
  doesFileExist  = lift . doesFileExist
  readSourceFile = lift . readSourceFile


runCompile
  :: Monad m
  => CompilerOptions
  -> CompilerFlags
  -> Compile m a
  -> m a
runCompile options flags action = action
  & flip runReaderT compileInfo
  & flip evalStateT compileContext
  where
    compileInfo    = CompileInfo options flags
    compileContext = CompileContext mempty mempty mempty mempty mempty mempty

addInterface
  :: Monad m
  => ModuleName
  -> Interface
  -> Compile m ()
addInterface moduleName interface@Interface {..} = do
  ccInterfaces       %= M.insert moduleName interface
  ccDefinitionCache <>= _interfaceDefinitions
  ccFunctionCache   <>= _interfaceFunctions
  ccSymbolCache     <>= _interfaceSymbols
