module Lang.Pietre.Export.Bytecode
  ( renderBytecode
  , renderBinary
  ) where

import "this" Prelude

import Data.HashMap.Strict.Extra              qualified as M
import Data.List.NonEmpty                     qualified as NE
import Data.Text                              qualified as T

import Lang.Pietre.Representations.Binary
import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.IR         (Label (..))
import Lang.Pietre.Representations.Name


renderBytecode :: Object -> Text
renderBytecode =
  T.unlines . intercalate [""] . map (uncurry renderFunction) . M.toList

renderBinary :: Binary -> Text
renderBinary Binary {..} =
  T.unlines $ intercalate [""] $ header : output
  where
    header = ["main: " <> T.show _bMainAddress]
    -- TODO: do not hardcode this 3
    output = flip evalState 3 $ traverse (uncurry renderLinkedFunction) $ zip [0..] $ toList _bFunctions


renderFunction :: Name -> InstructionBuffer -> [Text]
renderFunction name buffer =
  renderName name : map (("  " <>) . renderUnresolvedInstruction) (toList buffer)

renderLinkedFunction
  :: Int
  -> Function (Seq (Instruction Resolved))
  -> State Int [Text]
renderLinkedFunction index Function {..} = do
  output <- traverse renderResolvedInstruction $ toList _fInstructions
  pure $ T.show index : map ("  " <>) output

renderUnresolvedInstruction :: Instruction Unresolved -> Text
renderUnresolvedInstruction = \case
  PushAddr addr -> "PushAddr " <> renderAddress addr
  Entrance addr -> "Entrance " <> renderAddress addr
  instruction   -> T.show instruction

renderResolvedInstruction
  :: Instruction Resolved
  -> State Int Text
renderResolvedInstruction = \case
  Entrance _ -> do
    currentAddress <- get
    modify (+1)
    pure $ "Entrance " <> T.show currentAddress
  instruction ->
    pure $ T.show instruction


renderAddress :: Address -> Text
renderAddress (name, Label x y) =
  renderName name <> "@" <> T.show x <> "." <> T.show y

renderName :: Name -> Text
renderName Name {..} =
  case _nameParams of
    [] -> renderBaseName _nameBase
    ps -> renderBaseName _nameBase <> "<" <> T.intercalate "," (map renderName ps) <> ">"

renderBaseName :: BaseName -> Text
renderBaseName BaseName {..} =
  T.intercalate "::" (map rawIdentifier $ NE.toList _nameModule) <> "::" <> rawIdentifier _nameIdent
