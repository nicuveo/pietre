module Main where

import "this" Prelude                 hiding (readFile)

import Control.Exception              qualified as CE
import Data.Text.IO                   qualified as T
import Graphics.Image                 qualified as I
import System.Directory               qualified as SD
import System.Environment
import System.Exit

import Lang.Pietre
import Lang.Pietre.Internal.Diagnosis


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


newtype Run a = Run { run :: DiagnosisT IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadDiagnosis
    , MonadIO
    )

instance MonadFileSystem Run where
  doesFileExist = liftIO . SD.doesFileExist
  readSourceFile filePath = liftIO do
    CE.try (T.readFile filePath) <&> \case
      Left (_ :: CE.IOException) -> Nothing
      Right sourceCode           -> Just sourceCode

execute :: Run a -> IO (Seq Diagnostic, Maybe a)
execute action = run action
  & runDiagnosisT


main :: IO ()
main = do
  (diagnostics, result) <- execute do
    commandLineArgs <- liftIO getArgs
    parseCommand commandLineArgs >>= \case
      Help -> liftIO help
      Compile options flags mainFile -> do
        image <- compileBinary options flags mainFile
        liftIO $ I.writeImageExact I.PNG [] "program.png" image
  traverse_ print diagnostics
  maybe exitFailure (const exitSuccess) result
