{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Pipeline.Options where

import "this" Prelude

import Control.Lens
import Data.Sequence       qualified as Seq
import Options.Applicative


data CompilerOptions = CompilerOptions
  { _coVerbose      :: Bool
  , _coIncludePaths :: Seq FilePath
  , _coExportAST    :: Maybe FilePath
  , _coExportIR     :: Maybe FilePath
  , _coOutput       :: Maybe FilePath
  } deriving Show

data CompilerFlags = CompilerFlags
  { _cfSimplify :: Bool
  , _cfOptimize :: Bool
  , _cfMinimize :: Bool
  } deriving Show

makeLenses ''CompilerOptions
makeLenses ''CompilerFlags


optionsParser :: Parser (CompilerOptions, CompilerFlags, FilePath)
optionsParser = do
  _coVerbose <- switch $ mconcat
    [ short 'v'
    , long "verbose"
    , help "print debug options"
    ]
  _coOutput <- optional $ strOption $ mconcat
    [ short 'o'
    , long "output"
    , help "name of the output program"
    ]
  _coIncludePaths <- fmap Seq.fromList $ many $ strOption $ mconcat
    [ short 'I'
    , long "include"
    , help "folder in which to search for source files"
    ]
  _coExportAST <- optional $ strOption $ mconcat
    [ long "export-AST"
    , help "folder in which to export the verified AST"
    ]
  _coExportIR <- optional $ strOption $ mconcat
    [ long "export-IR"
    , help "folder in which to export the IR"
    ]
  _cfSimplify <- boolOptionParser True  "simplify"
  _cfOptimize <- boolOptionParser False "optimize"
  _cfMinimize <- boolOptionParser True  "minimize"
  mainFile <- strArgument $ mconcat
    [ help "main program file"
    ]
  pure (CompilerOptions {..}, CompilerFlags {..}, mainFile)
  where
    boolOptionParser :: Bool -> String -> Parser Bool
    boolOptionParser defaultValue name = asum
      [ flag' True (long name)
      , flag' False (long $ "no-" ++ name)
      , pure defaultValue
      ]
