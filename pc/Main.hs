module Main where

import "this" Prelude hiding (getContents, putStrLn)

import Data.Text.IO   (getContents, putStrLn)
import Lang.Pietre
import System.Exit


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

main :: IO ()
main = do
  source <- getContents
  ast <- parseModule "<interactive>" source `onLeft` const help
  putStrLn $ prettyPrint ast
