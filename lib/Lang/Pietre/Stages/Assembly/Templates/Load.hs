module Lang.Pietre.Stages.Assembly.Templates.Load where

import "this" Prelude

import Data.Char
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath


templatePath :: [FilePath]
templatePath =
  [ "lib"
  , "Lang"
  , "Pietre"
  , "Stages"
  , "Assembly"
  , "Templates"
  ]

generateTemplates :: Name -> Q [Dec]
generateTemplates templateFunction = do
  rootDir <- qRunIO getCurrentDirectory
  let templateDir = joinPath $ rootDir : templatePath
  allFiles <- qRunIO (listDirectory templateDir)
  catMaybes <$> for allFiles \filename -> do
    let filepath = templateDir </> filename
    isDir <- qRunIO $ doesDirectoryExist filepath
    if | takeExtension filename /= ".tmp" -> pure Nothing
       | isDir                            -> pure Nothing
       | otherwise                        -> do
           templateName <- newName $
             map toLower (dropExtensions filename) ++ "Template"
           body <- AppE (VarE templateFunction) <$> embedFile filepath
           pure $ Just $ ValD (VarP templateName) (NormalB body) []
