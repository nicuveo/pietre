module Lang.Pietre.Export.PrettyPrinting.AST.Parsed (prettyPrint) where

import "this" Prelude


import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Export.PrettyPrinting.AST.Parsed.Rendering
import Lang.Pietre.Representations.AST.Parsed


prettyPrint :: Module -> Text
prettyPrint = renderModule
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> renderStrict
