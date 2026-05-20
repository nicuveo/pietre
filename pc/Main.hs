module Main where

import "this" Prelude

import Control.Exception                            qualified as CE
import Data.Aeson                                   (encode)
import Data.ByteString.Lazy                         qualified as BS
import Data.Text.IO                                 qualified as T
import Graphics.Image                               qualified as I
import Options.Applicative                          hiding (action)
import System.Directory                             qualified as SD
import System.Exit
import System.FilePath

import Lang.Pietre
import Lang.Pietre.Export.JSON.Diagnostic
import Lang.Pietre.Export.PrettyPrinting.Diagnostic
import Lang.Pietre.Internal.Diagnosis


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
  writeToFile filePath fileContent = liftIO do
    SD.createDirectoryIfMissing True $ takeDirectory filePath
    T.writeFile filePath fileContent

execute :: Run a -> IO (Seq Diagnostic, Maybe a)
execute action = run action
  & runDiagnosisT

programOptions :: ParserInfo (CompilerOptions, CompilerFlags, FilePath)
programOptions = info (optionsParser <**> helper) $ mconcat
  [ fullDesc
  , header "Pietre compiler"
  ]

main :: IO ()
main = do
  (options, flags, mainFile) <- execParser programOptions
  (diagnostics, result) <- execute do
    image <- compileBinary options flags mainFile
    let programName = fromMaybe "program.png" $ _coOutput options
    liftIO $ I.writeImageExact I.PNG [] programName image
  if _coJSONDiagnostics options
  then
    BS.putStr $ encode $ serialize diagnostics
  else
    traverse_ (T.putStrLn . prettyPrint) diagnostics
  maybe exitFailure (const exitSuccess) result
