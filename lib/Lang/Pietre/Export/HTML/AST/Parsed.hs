module Lang.Pietre.Export.HTML.AST.Parsed (renderHTML) where

import "this" Prelude


import Lucid
import Prettyprinter
import Prettyprinter.Lucid
import Prettyprinter.Render.Util.SimpleDocTree

import Lang.Pietre.Export.HTML.Common
import Lang.Pietre.Export.PrettyPrinting.AST.Parsed.Rendering
import Lang.Pietre.Representations.AST.Parsed


--------------------------------------------------------------------------------
-- API

renderHTML :: Module -> Text
renderHTML = renderModule
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> fmap htmlAnnotation
  >>> treeForm
  >>> renderHtml
  >>> renderToText


--------------------------------------------------------------------------------
-- Internal helpers

htmlAnnotation :: Annotation -> Html () -> Html ()
htmlAnnotation = \case
  IntLiteralAnn    -> span_ [class_ "syntax-parsed-int-literal"]
  CharLiteralAnn   -> span_ [class_ "syntax-parsed-string-literal"]
  StringLiteralAnn -> span_ [class_ "syntax-parsed-string-literal"]
  KeywordAnn       -> span_ [class_ "syntax-parsed-keyword"]
  PathAnn          -> span_ [class_ "syntax-parsed-path"]
