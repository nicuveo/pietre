{-# LANGUAGE OverloadedLists #-}

module Lang.Pietre.Stages.Minimization (minimize) where

import "this" Prelude

import Control.Applicative
import Data.HashSet                         qualified as S
import Data.List                            qualified as L
import Data.Sequence                        as Seq

import Lang.Pietre.Representations.Bytecode


minimize
  :: InstructionBuffer
  -> InstructionBuffer
minimize instructions = go instructions rules
  where
    go buffer = \case
      []     -> buffer
      (r:rs) -> case r buffer of
        Just buffer' -> go buffer' rules
        Nothing      -> go buffer  rs


type Rule = InstructionBuffer -> Maybe (InstructionBuffer)

rules :: [Rule]
rules =
  [ mergeRolls
  , removeRolls
  , reorganizeStack
  , removeRedundantJumps
  , removeUnusedEntrances
  , replacePushByDuplicate
  ]


replacePushByDuplicate :: Rule
replacePushByDuplicate = applyOnAllSuffixes $ segmented 2 \case
  [PushInt x, PushInt y]
    | x == y
    -> Just [PushInt x, Duplicate]
  _ -> Nothing

mergeRolls :: Rule
mergeRolls = applyOnAllPrefixes $ segmented 6 \case
  [PushInt depth1, PushInt steps1, Roll, PushInt depth2, PushInt steps2, Roll]
    | depth1 == depth2
    -> Just [PushInt depth1, PushInt (steps1 + steps2), Roll]
  _ -> Nothing

removeRolls :: Rule
removeRolls = applyOnAllPrefixes $ segmented 3 \case
  [PushInt depth, PushInt steps, Roll]
    | steps == 0 || steps == depth
    -> Just []
  _ -> Nothing

reorganizeStack :: Rule
reorganizeStack = applyOnAllPrefixes \s -> case Seq.spanl isPush s of
  (viewr -> (viewr -> lhs :> PushInt depth) :> PushInt steps, viewl -> Roll :< rhs)
    | depth <= Seq.length lhs
    -> Just $ roll depth steps lhs <> rhs
  _ -> Nothing
  where
    isPush = \case
      PushInt  _ -> True
      PushAddr _ -> True
      _          -> False
    roll depth steps s =
      let (lhs, rolled) = Seq.splitAt (Seq.length s - depth) s
          (segment1, segment2) = Seq.splitAt (depth - steps) rolled
      in lhs <> segment2 <> segment1

removeRedundantJumps :: Rule
removeRedundantJumps = applyOnAllPrefixes $ segmented 3 \case
  [PushAddr addr1, Return, Entrance addr2]
    | addr1 == addr2
    -> Just [Entrance addr2]
  _ -> Nothing

removeUnusedEntrances :: Rule
removeUnusedEntrances s =
  let
    (jumps, entrances) = L.foldl' collectAddresses (S.empty, S.empty) $ Seq.drop 1 s
    unused = S.difference entrances jumps
  in
    if S.null unused then Nothing else Just $ Seq.filter (not . mustBeRemoved unused) s
  where
    collectAddresses accum@(jumps, entrances) = \case
      PushAddr addr -> (S.insert addr jumps, entrances)
      Entrance addr -> (jumps, S.insert addr entrances)
      _             -> accum
    mustBeRemoved unused = \case
      Entrance addr | addr `S.member` unused -> True
      _ -> False


applyOnAllPrefixes :: Rule -> Rule
applyOnAllPrefixes rule = go Seq.Empty
  where
    go lhs s = case rule s of
      Just newSeq -> Just (lhs <> newSeq)
      Nothing     -> case viewl s of
        EmptyL    -> Nothing
        x :< rest -> go (lhs |> x) rest

applyOnAllSuffixes :: Rule -> Rule
applyOnAllSuffixes rule = go Seq.Empty
  where
    go lhs s = case viewl s of
      EmptyL    -> Nothing
      x :< rest -> go (lhs |> x) rest <|> fmap (lhs <>) (rule s)

segmented :: Int -> Rule -> Rule
segmented n rule s =
  let (segment1, segment2) = Seq.splitAt n s
  in fmap (<> segment2) $ rule segment1
