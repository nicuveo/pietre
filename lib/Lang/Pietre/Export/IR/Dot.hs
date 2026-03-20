module Lang.Pietre.Export.IR.Dot (renderIR) where

import "this" Prelude

import Data.HashMap.Strict.Extra              qualified as M
import Data.List.NonEmpty                     qualified as NE
import Data.Text                              qualified as T
import Text.Dot                               hiding (target)
import TextBuilder                            qualified as TB

import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.IR
import Lang.Pietre.Representations.Name


renderIR :: IR -> Text
renderIR ir = TB.toText $ digraph do
  defaults Cluster .= M.fromList
    [ ("labeljust", "l")
    , ("fontsize",  "16")
    , ("fontname",  "Helvetica-Oblique")
    , ("bgcolor",   "#D9EdF8")
    ]
  defaults Node .= M.fromList
    [ ("fontsize",  "16")
    , ("fontname",  "Helvetica")
    , ("style",     "filled")
    , ("fillcolor", "#FDFDFF")
    ]
  defaults Edge .= M.fromList
    [ ("fontsize",  "16")
    , ("fontname",  "Helvetica")
    ]
  M.traverseWithKey_ visitFunction ir

visitFunction :: Name -> Function -> Dot ()
visitFunction functionName (_funBlocks -> blocks) = do
  let functionText = renderName functionName
  cluster do
    its label ?= functionText
    for_ blocks \(blockLabel, Block {..}) -> do
      thisBlock <- node $ T.concat $ map (<> "\\l") $ concat
        [ [ renderTitle blockLabel
          , renderArguments _blockArguments
          , ""
          ]
        , map renderInstruction _blockInstructions
        , pure $ renderTerminator _blockTerminator
        ]
      registerItAs $ functionText <> "@" <> renderLabel blockLabel
      its shape ?= "rectangle"
      case _blockTerminator of
        Jump target -> do
          edge thisBlock $ retrieve $ functionText <> "@" <> renderLabel (_tgtLabel target)
          pass
        Branch ifTrue ifFalse _ -> do
          edge thisBlock $ retrieve $ functionText <> "@" <> renderLabel (_tgtLabel ifTrue)
          its label ?= "true"
          edge thisBlock $ retrieve $ functionText <> "@" <> renderLabel (_tgtLabel ifFalse)
          its label ?= "false"
        Return _ ->
          pass
        Panic ->
          pass

renderTitle :: Label -> Text
renderTitle lbl = "Block: " <> renderLabel lbl

renderLabel :: Label -> Text
renderLabel Label {..} = "l" <> T.show _labelBlock

renderArguments :: [Register] -> Text
renderArguments args = "Args: " <> T.intercalate ", " (map renderRegister args)

renderRegister :: Register -> Text
renderRegister arg = "%" <> T.show (_registerIndex arg)

renderInstruction :: Instruction -> Text
renderInstruction = \case
  Add target reg1 reg2 ->
    T.unwords [renderRegister target, "= Add", renderRegister reg1, renderRegister reg2]
  Subtract target reg1 reg2 ->
    T.unwords [renderRegister target, "= Subtract", renderRegister reg1, renderRegister reg2]
  Multiply target reg1 reg2 ->
    T.unwords [renderRegister target, "= Multiply", renderRegister reg1, renderRegister reg2]
  Divide target reg1 reg2 ->
    T.unwords [renderRegister target, "= Divide", renderRegister reg1, renderRegister reg2]
  Modulo target reg1 reg2 ->
    T.unwords [renderRegister target, "= Modulo", renderRegister reg1, renderRegister reg2]
  Exponent target reg1 reg2 ->
    T.unwords [renderRegister target, "= Exponent", renderRegister reg1, renderRegister reg2]
  CmpEQ target reg1 reg2 ->
    T.unwords [renderRegister target, "= CmpEQ", renderRegister reg1, renderRegister reg2]
  CmpNE target reg1 reg2 ->
    T.unwords [renderRegister target, "= CmpNE", renderRegister reg1, renderRegister reg2]
  CmpLT target reg1 reg2 ->
    T.unwords [renderRegister target, "= CmpLT", renderRegister reg1, renderRegister reg2]
  CmpLE target reg1 reg2 ->
    T.unwords [renderRegister target, "= CmpLE", renderRegister reg1, renderRegister reg2]
  CmpGT target reg1 reg2 ->
    T.unwords [renderRegister target, "= CmpGT", renderRegister reg1, renderRegister reg2]
  CmpGE target reg1 reg2 ->
    T.unwords [renderRegister target, "= CmpGE", renderRegister reg1, renderRegister reg2]
  NegateI target reg ->
    T.unwords [renderRegister target, "= NegateI", renderRegister reg]
  NegateB target reg ->
    T.unwords [renderRegister target, "= NegateB", renderRegister reg]
  AssignI target value ->
    T.unwords [renderRegister target, "=", T.show value]
  AssignB target value ->
    T.unwords [renderRegister target, "=", T.show value]
  AssignC target value ->
    T.unwords [renderRegister target, "=", T.replace "\\" "\\\\" (T.show value)]
  AssignA target value ->
    T.unwords [renderRegister target, renderName value]
  InvokeN Nothing funcName args ->
    renderName funcName <> "(" <> T.intercalate ", " (map renderRegister args) <> ")"
  InvokeN (Just target) funcName args ->
    renderRegister target <> " = " <> renderName funcName <> "(" <> T.intercalate ", " (map renderRegister args) <> ")"
  _ -> "unimplemented"

renderTerminator :: Terminator -> Text
renderTerminator = \case
  Jump target ->
    "Jump " <> renderTarget target
  Branch ifTrue ifFalse reg ->
    "Branch " <> T.unwords [renderTarget ifTrue, renderTarget ifFalse, renderRegister reg]
  Return Nothing ->
    "Return"
  Return (Just value) ->
    "Return " <> renderRegister value
  Panic ->
    "Panic"

renderTarget :: Target -> Text
renderTarget Target {..} = renderLabel _tgtLabel <> "(" <> T.intercalate ", " (map renderRegister _tgtArgs) <> ")"

renderName :: Name -> Text
renderName Name {..} =
  case _nameParams of
    [] -> renderBaseName _nameBase
    ps -> renderBaseName _nameBase <> "<" <> T.intercalate "," (map renderName ps) <> ">"

renderBaseName :: BaseName -> Text
renderBaseName BaseName {..} =
  T.intercalate "::" (map rawIdentifier $ NE.toList _nameModule) <> "::" <> rawIdentifier _nameIdent
