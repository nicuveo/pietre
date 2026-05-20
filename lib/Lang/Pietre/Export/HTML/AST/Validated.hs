module Lang.Pietre.Export.HTML.AST.Validated (renderHTML) where

import "this" Prelude

import Lucid                                                     hiding (for_)
import Lucid.Base                                                (makeAttribute)
import Prettyprinter
import Prettyprinter.Lucid
import Prettyprinter.Render.Util.SimpleDocTree

import Lang.Pietre.Export.HTML.Common
import Lang.Pietre.Export.PrettyPrinting.AST.Validated.Rendering
import Lang.Pietre.Representations.Interface


--------------------------------------------------------------------------------
-- API

renderHTML :: Interface -> Text
renderHTML = renderInterface
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> fmap htmlAnnotation
  >>> treeForm
  >>> renderHtml
  >>> renderToText


--------------------------------------------------------------------------------
-- Internal helpers

htmlAnnotation :: Annotation -> Html () -> Html ()
htmlAnnotation = \case
  IntLiteralAnn      -> span_ [class_ "syntax-validated-int-literal"]
  CharLiteralAnn     -> span_ [class_ "syntax-validated-string-literal"]
  StringLiteralAnn   -> span_ [class_ "syntax-validated-string-literal"]
  KeywordAnn         -> span_ [class_ "syntax-validated-keyword"]
  DeclarationAnn uid -> span_ [makeAttribute "declID" uid, id_ uid]
  VariableAnn uid    -> mkReference uid "syntax-validated-variable"
  ReferenceAnn uid   -> mkReference uid "syntax-validated-reference"
  TypeAnn uid        -> mkReference uid "syntax-validated-type"
  FunctionAnn uid    -> mkReference uid "syntax-validated-function"
  ParameterAnn uid   -> mkReference uid "syntax-validated-parameter"
  where
    mkReference uid className =
      span_ [makeAttribute "declid" uid, class_ className] . a_ [href_ $ "#" <> uid]
