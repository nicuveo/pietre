{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Assembly where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra                   (whenJustM)
import Data.Monoid
import Graphics.Image                        qualified as I

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Stages.Assembly.Color
import Lang.Pietre.Stages.Assembly.Templates


data AssemblyState = AssemblyState
  { _asCurrentColor :: Color
  , _asCurrentImage :: Image
  , _asLastEntrance :: Last Int
  }


initialState :: Color -> Image -> AssemblyState
initialState color image = AssemblyState color image mempty

addFunction :: (Int, Image) -> (Int, (Int, Image)) -> (Int, Image)
addFunction (stripHeight, baseImage) (fcount, (entrances, function)) =
  (stripHeight, appEndo (mconcat allTransforms) baseImage)
  where
    referenceColumn = 10 * fcount + 1
    allTransforms = map Endo $
      [ applyTemplate (stripHeight + 1, referenceColumn) function
      , applyTemplate (1, referenceColumn + 4) (strip0Template Red)
      ] ++ do
        stripIndex <- [2..entrances]
        pure $ applyTemplate (stripIndex, referenceColumn + 4) (strip1Template Red)

makeLenses 'AssemblyState

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
        for_ [currentRow - 5 .. prevEntrance] $
          const $ expand 0 blankTemplate
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

resolvePush :: Instruction Resolved -> [Instruction Resolved]
resolvePush = \case
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
          go d ++ [PushInt 16, Multiply] ++
          if r > 0 then [PushInt r, Add] else []

generateFunctionImage :: [Instruction Resolved] -> Image
generateFunctionImage instructions = _asCurrentImage $
  execState
    (go $ concatMap resolvePush instructions)
    (initialState Green $ functionTemplate Green)
  where
    go = \case
      []      -> pure ()
      [i]     -> appendInstruction Nothing i
      (i:j:l) -> do
        case j of
          PushInt n -> appendInstruction (Just n) i
          _         -> appendInstruction Nothing  i
        go (j:l)

assemble :: [(Int, [Instruction Resolved])] -> Image
assemble functions = snd
  $ foldl' addFunction (stripHeight, initialImage)
  $ zip [0..] images
  where
    images = map (fmap generateFunctionImage) functions
    stripHeight = maximum (map fst images)
    maxHeight = maximum (map (I.rows . snd) images) + stripHeight + 2
    maxWidth = 10 * length functions + 4
    initialImage = applyTemplate (0,0) (cornerTemplate Red)
      $ I.makeImage (maxHeight, maxWidth)
      $ const
      $ I.PixelRGBA 0xFF 0xFF 0xFF 0xFF
