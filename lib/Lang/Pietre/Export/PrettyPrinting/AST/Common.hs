module Lang.Pietre.Export.PrettyPrinting.AST.Common where

import "this" Prelude

import Prettyprinter


sepByColons :: [Doc ann] -> Doc ann
sepByColons = concatWith \x y -> x <> "::" <> y

sepByCommas :: [Doc ann] -> Doc ann
sepByCommas = concatWith \x y -> x <> "," <+> y

sepByNewlines :: [Doc ann] -> Doc ann
sepByNewlines = concatWith \x y -> x <> hardline <> hardline <> y

sepEndByNewlines :: [Doc ann] -> Doc ann
sepEndByNewlines = sepByNewlines >>> (<> hardline)
