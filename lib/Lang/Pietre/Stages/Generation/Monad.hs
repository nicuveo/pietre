{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Generation.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                  qualified as M
import Data.HashSet                         qualified as S
import Data.Sequence                        qualified as Seq

import Lang.Pietre.Representations.Bytecode as BC
import Lang.Pietre.Representations.IR       as IR
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Generation.Stack  (Stack)
import Lang.Pietre.Stages.Generation.Stack  qualified as Stack


type Generate = ReaderT GenerationInfo (State GenerationContext)

type InstructionBuffer = Seq (BC.Instruction Unresolved)

data GenerationInfo = GenerationInfo
  { _giFunctionName :: Name
  }

data GenerationContext = GenerationContext
  { _gcInstructions :: InstructionBuffer
  , _gcStack        :: Stack
  , _gcSubLabel     :: Label
  }

makeLenses ''GenerationInfo
makeLenses ''GenerationContext


runGeneration :: Name -> Generate a -> a
runGeneration functionName action = action
  & flip runReaderT (GenerationInfo functionName)
  & flip evalState  (GenerationContext [] [] (Label 0 0))


currentStack :: Generate [Register]
currentStack = uses gcStack Stack.toList

stackSize :: Generate Int
stackSize = uses gcStack Stack.size


generateWith :: Label -> [Register] -> Generate a -> Generate InstructionBuffer
generateWith label startingStack action = do
  gcInstructions .= Seq.empty
  gcStack .= Stack.fromList startingStack
  gcSubLabel .= label
  void action
  use gcInstructions

withTempContext :: [Register] -> Generate a -> Generate a
withTempContext startingStack action = do
  stack  <- use gcStack
  buffer <- use gcInstructions
  gcInstructions .= Seq.empty
  gcStack .= Stack.fromList startingStack
  result <- action
  gcInstructions .= buffer
  gcStack .= stack
  pure result

appendInstructions :: InstructionBuffer -> Generate ()
appendInstructions = (<>=) gcInstructions

appendPush :: Register -> InstructionBuffer -> Generate ()
appendPush register instructions = do
  appendInstructions instructions
  gcStack %= Stack.push register

appendRoll :: Int -> Int -> Generate ()
appendRoll depth steps
  | depth == 0     = pass
  | depth == steps = pass
  | otherwise = do
     gcInstructions <>= [PushInt depth, PushInt steps, Roll]
     gcStack %= Stack.roll depth steps

appendFullStackRoll :: Int -> Generate ()
appendFullStackRoll steps = do
  depth <- stackSize
  appendRoll depth steps

appendOperation
  :: HashSet Register
  -> Register
  -> [Register]
  -> InstructionBuffer
  -> Generate ()
appendOperation outputRegisters target args bytecode = do
  appendRearrangeArgs outputRegisters args
  gcInstructions <>= bytecode
  replicateM_ (length args) $ gcStack %= Stack.pop
  gcStack %= Stack.push target

appendRearrangeArgs
  :: HashSet Register
  -> [Register]
  -> Generate ()
appendRearrangeArgs outputRegisters args = do
  stack <- currentStack
  appendRearrangeStack $ Stack.fromList $ args ++ filter (`S.member` outputRegisters) stack

appendRearrangeStack :: [Register] -> Generate ()
appendRearrangeStack desiredStack = do
  stackContents <- uses gcStack Stack.contents
  let desiredContents = M.fromListWith (+) $ map (,1) desiredStack
  adjustStackContent desiredContents stackContents
  traverse_ (uncurry rollStackRegister) $ reverse $ zip [0..] desiredStack
  where
    adjustStackContent
      :: HashMap Register Int
      -> HashMap Register Int
      -> Generate ()
    adjustStackContent desiredContent currentContent
      | currentContent == desiredContent = pass
      | otherwise = do
          uses gcStack Stack.head >>= \case
            Nothing -> pass
            Just r  -> do
              let delta = (M.lookupDefault 0 r desiredContent) - (currentContent M.! r)
              case compare delta 0 of
                GT -> do
                  replicateM_ delta do
                    -- TODO: handle bigger registers
                    gcInstructions <>= [BC.Duplicate]
                    gcStack %= Stack.push r
                  appendFullStackRoll (delta+1)
                  adjustStackContent desiredContent $ M.adjust (+delta) r currentContent
                LT -> do
                  -- TODO: handle bigger registers
                  gcInstructions <>= [BC.Pop]
                  gcStack %= Stack.pop
                  adjustStackContent desiredContent $ M.update subtractOrDelete r currentContent
                EQ -> do
                  -- TODO: handle bigger registers
                  appendFullStackRoll 1
                  adjustStackContent desiredContent currentContent

    subtractOrDelete 1 = Nothing
    subtractOrDelete x = Just (x-1)

    rollStackRegister :: Int -> Register -> Generate ()
    rollStackRegister targetIndex r = do
      currentRegister <- uses gcStack $ Stack.inspect targetIndex
      if currentRegister == Just r
      then pass
      else do
        firstIndex <- uses gcStack $ Stack.find r
        appendRoll (targetIndex+1) (firstIndex+1)


generateRearrangeStack :: [Register] -> [Register] -> Generate InstructionBuffer
generateRearrangeStack startingStack desiredStack = do
  withTempContext startingStack do
    appendRearrangeStack desiredStack
    use gcInstructions

generateAddress :: Generate (Label, InstructionBuffer)
generateAddress = do
  Label x y <- use gcSubLabel
  let newLabel = Label x (y+1)
  gcSubLabel .= newLabel
  functionName <- view giFunctionName
  pure (newLabel, [BC.Entrance (functionName, newLabel)])

generateJump :: Label -> Generate InstructionBuffer
generateJump target = do
  functionName <- view giFunctionName
  pure [BC.PushAddr (functionName, target), BC.Return]

generateBranch :: Label -> Generate InstructionBuffer
generateBranch target = do
  functionName <- view giFunctionName
  pure
    [ BC.PushAddr (functionName, target)
    , BC.PushInt 2
    , BC.PushInt 1
    , BC.Roll
    , BC.Branch
    , BC.Pop
    ]


{-
lastNInstructions :: Int -> Generate [BC.Instruction Unresolved]
lastNInstructions amount = go amount [] =<< uses gcInstructions Seq.viewr
  where
    go 0  !buf = buf
    go !n !buf = \case
      Seq.EmptyR   -> buf
      s (Seq.:>) x -> go (n-1) (x : buf) s
-}
