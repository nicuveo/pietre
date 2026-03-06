{-# LANGUAGE OverloadedLists #-}

module Lang.Pietre.Stages.Assembly where

import "this" Prelude

import Control.Lens                          hiding ((<|), (|>))
import Control.Monad.Extra                   (whenJustM)
import Data.Monoid
import Data.Sequence                         ((<|), (|>))
import Data.Sequence                         qualified as Seq
import Graphics.Image                        qualified as I

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Binary
import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Stages.Assembly.Color
import Lang.Pietre.Stages.Assembly.Monad
import Lang.Pietre.Stages.Assembly.Templates

import Debug.Trace


assemble :: Binary -> Image
assemble Binary {..} =
  Seq.foldlWithIndex (addFunction stripHeight) initialImage images
  where
    startFunction = generateStartFunction _bMainAddress
    images = fmap generateFunctionImage $ startFunction <| _bFunctions
    stripHeight = maximum $ fmap _fEntranceCount images
    maxHeight = maximum (fmap (I.rows . _fInstructions) images) + stripHeight + 2
    maxWidth = 10 * (length _bFunctions + 1) + 4
    initialImage = applyTemplate (0,0) (cornerTemplate Red)
      $ I.makeImage (maxHeight, maxWidth)
      $ const
      $ I.PixelRGBA 0xFF 0xFF 0xFF 0xFF


generateStartFunction
  :: Int
  -> Function (Seq (Instruction Resolved))
generateStartFunction mainAddress = Function
  { _fEntranceCount    = 2
  , _fFunctionEntrance = 1
  , _fInstructions =
    [ Entrance ()
    , PushInt 2
    , PushInt mainAddress
    , Return
    , Entrance ()
    , Terminate
    ]
  }

generateFunctionImage
  :: Function (Seq (Instruction Resolved))
  -> Function Image
generateFunctionImage function@Function {..} =
  let image = runAssembly Green do
        traceShowM _fInstructions
        let expandedInstructions = expandInstruction =<< _fInstructions
        traceShowM expandedInstructions
        sequence_ $ Seq.zipWith go expandedInstructions (Seq.drop 1 expandedInstructions |> Return)
        use asCurrentImage
  in function { _fInstructions = image }
  where
    go instruction = \case
      PushInt n -> appendInstruction (Just n) instruction
      _         -> appendInstruction Nothing  instruction

expandInstruction
  :: Instruction Resolved
  -> Seq (Instruction Resolved)
expandInstruction = \case
  PushInt n
    | n <= 0    -> go 1 <> go (abs n + 1) <> [Subtract]
    | otherwise -> go n
  instruction -> pure instruction
  where
    go 0 = []
    go n
      | n <= 16   = [PushInt n]
      | n <= 32   = [PushInt 16, PushInt (n-16)]
      | otherwise = let (d, r) = n `divMod` 16 in
          go d <> [PushInt 16, Multiply] <>
          if r > 0 then [PushInt r, Add] else []

appendInstruction
  :: Maybe Int
  -> Instruction Resolved
  -> State AssemblyState ()
appendInstruction size instruction =
  case instruction of
    Pop        -> sizedInstruction
    Add        -> sizedInstruction
    Subtract   -> sizedInstruction
    Multiply   -> sizedInstruction
    Divide     -> sizedInstruction
    Mod        -> sizedInstruction
    Not        -> sizedInstruction
    Greater    -> sizedInstruction
    Duplicate  -> sizedInstruction
    Roll       -> sizedInstruction
    InInt      -> sizedInstruction
    InChar     -> sizedInstruction
    OutInt     -> sizedInstruction
    OutChar    -> sizedInstruction
    PushInt _  -> sizedInstruction
    Return     -> expand 1 exitTemplate
    Terminate  -> expand 0 terminateTemplate
    PushAddr _ -> unimplemented
    Entrance _ -> do
      whenJustM (uses asLastEntrance getLast) \prevEntrance -> do
        currentRow <- uses asCurrentImage I.rows
        replicateM_ (prevEntrance + 6 - currentRow) $
          expand 0 blankTemplate
      expand 2 entranceTemplate
      appendInstruction size Subtract
    Branch -> do
      expand 1 branchTemplate
      appendInstruction size Subtract
  where
    sizedInstruction = case size of
      Nothing -> expand 0 instructionTemplate
      Just s  -> push s

    expand offset f = do
      prevColor <- use asCurrentColor
      prevImage <- use asCurrentImage
      let newColor = nextColor prevColor instruction
          template = f newColor
          newImage = prevImage
            & I.canvasSize I.Edge (I.rows prevImage + I.rows template - offset, 10)
            & applyTemplate (I.rows prevImage - offset, 0) template
      asCurrentColor .= newColor
      asCurrentImage .= newImage

    push 1 = expand 0 push1Template
    push 2 = expand 0 push2Template
    push 3 = expand 0 push3Template
    push 4 = expand 0 push4Template
    push n = do
      prevColor <- use asCurrentColor
      push (n-4)
      asCurrentColor .= prevColor
      expand 0 push4Template

addFunction
  :: Int
  -> Image
  -> Int
  -> Function Image
  -> Image
addFunction stripHeight baseImage fcount Function {..} =
  appEndo (mconcat allTransforms) baseImage
  where
    referenceColumn = 10 * fcount + 1
    allTransforms = map Endo $
      [ applyTemplate (stripHeight + 1, referenceColumn) _fInstructions
      , applyTemplate (1, referenceColumn + 4) (strip0Template Red)
      ] ++ do
        stripIndex <- [2 .. _fEntranceCount]
        pure $ applyTemplate (stripIndex, referenceColumn + 4) (strip1Template Red)
