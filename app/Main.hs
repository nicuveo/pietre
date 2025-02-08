import "this" Prelude

import System.Exit

-- import Lang.Pietre


help :: IO a
help = do
  putStrLn "usage:\
    \\n\
    \\n    pietre [input-file-or-options]\
    \\n\
    \\noptions:\
    \\n    --help,-h                 display this help\
    \\n    -o name                   name of the output file\
    \\n    --format,-t [format]      output format (default: ppm)"
  exitFailure

main :: IO ()
main = help
