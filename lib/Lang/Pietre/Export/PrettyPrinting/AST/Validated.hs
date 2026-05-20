module Lang.Pietre.Export.PrettyPrinting.AST.Validated
  ( prettyPrint
  , prettyPrintExpression
  ) where

import "this" Prelude

import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Export.PrettyPrinting.AST.Validated.Rendering
import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Interface


prettyPrint :: Interface -> Text
prettyPrint = renderInterface
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> renderStrict

prettyPrintExpression :: Typed Expression -> Text
prettyPrintExpression = renderExpression
  >>> layoutSmart (LayoutOptions Unbounded)
  >>> renderStrict
