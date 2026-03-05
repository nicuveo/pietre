module Lang.Pietre.Stages.Generation.Stack
  ( -- * type
    Stack
    -- * accessors
  , size
  , contents
  , inspect
  , head
  , find
    -- * modification
  , push
  , pop
  , roll
    -- * conversion to/from list
  , toList
  , fromList
  ) where

import "this" Prelude                 hiding (find, head, toList)

import Data.HashMap.Strict            qualified as M
import Data.Sequence                  ((<|))
import Data.Sequence                  qualified as S
import GHC.IsList

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.IR


newtype Stack = Stack (Seq Register)
  deriving (Show)

instance IsList Stack where
  type Item Stack = Register
  fromListN = Stack ... fromListN
  fromList  = Stack .   fromList
  toList (Stack stack) = toList stack


size :: Stack -> Int
size (Stack stack) = S.length stack

contents :: Stack -> HashMap Register Int
contents (Stack stack) = foldl' step M.empty stack
  where
    step hmap r = M.insertWith (+) r 1 hmap

inspect :: Int -> Stack -> Maybe Register
inspect index (Stack stack) = S.lookup index stack

head :: Stack -> Maybe Register
head = inspect 0

find :: Register -> Stack -> Int
find r (Stack stack) =
  fromMaybe panic $ S.elemIndexL r stack
  where
    panic = reportICE
      "Stack.find"
      "could not find register"
      ["register: " ++ show r, "stack: " ++ show stack]


push :: Register -> Stack -> Stack
push r (Stack stack) = Stack (r <| stack)

pop :: Stack -> Stack
pop (Stack stack) = Stack $ S.drop 1 stack

roll :: Int -> Int -> Stack -> Stack
roll depth steps (Stack stack) =
  let
    (top,      bottom)   = S.splitAt depth stack
    (segment1, segment2) = S.splitAt steps top
  in
    Stack $ segment2 <> segment1 <> bottom
