{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedLists #-}

module Integration.Minimization
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Representations.IR       (Label (..))
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Minimization


test = testGroup "merge rolls"
  [ run
      "merge simple roll"
      [PushInt 3, PushInt 1, Roll, PushInt 3, PushInt 1, Roll]
      [PushInt 3, PushInt 2, Roll]
  , run
      "do not merge rolls of different depth"
      [PushInt 3, PushInt 1, Roll, PushInt 4, PushInt 1, Roll]
      [PushInt 3, PushInt 1, Roll, PushInt 4, PushInt 1, Roll]
  , run
      "do not merge rolls separated by instruction"
      [PushInt 3, PushInt 1, Roll, Return, PushInt 3, PushInt 1, Roll]
      [PushInt 3, PushInt 1, Roll, Return, PushInt 3, PushInt 1, Roll]
  , run
      "merge multiple rolls"
      [PushInt 5, PushInt 1, Roll, PushInt 5, PushInt 1, Roll, PushInt 5, PushInt 1, Roll]
      [PushInt 5, PushInt 3, Roll]
  ]

test = testGroup "remove rolls"
  [ run
      "remove empty roll (0)"
      [PushInt 3, PushInt 0, Roll]
      []
  , run
      "remove simple roll (depth=steps)"
      [PushInt 3, PushInt 3, Roll]
      []
  , run
      "remove simple roll (depth=N*steps)"
      [PushInt 3, PushInt 6, Roll]
      []
  , run
      "do not remove valid roll"
      [PushInt 6, PushInt 3, Roll]
      [PushInt 6, PushInt 3, Roll]
  , run
      "remove multiple rolls"
      [PushInt 3, PushInt 0, Roll, PushInt 3, PushInt 0, Roll]
      []
  ]

test = testGroup "reorganize stack"
  [ run
      "reorganize simple stack"
      [InChar, Duplicate, PushInt 1, PushInt 2, PushInt 3, PushInt 3, PushInt 2, Roll]
      [InChar, Duplicate, PushInt 2, PushInt 3, PushInt 1]
  , run
      "reorganize simple stacks"
      [PushInt 1, PushInt 2, PushInt 3, PushInt 3, PushInt 2, Roll, PushInt 1, PushInt 2, PushInt 3, PushInt 3, PushInt 2, Roll]
      [PushInt 2, PushInt 3, PushInt 1, PushInt 2, PushInt 3, PushInt 1]
  , run
      "do not reorganize stacks containing non-pushes"
      [InChar, Duplicate, PushInt 1, PushInt 2, PushInt 3, PushInt 4, PushInt 2, Roll]
      [InChar, Duplicate, PushInt 1, PushInt 2, PushInt 3, PushInt 4, PushInt 2, Roll]
  , run
      "only reorganize meaningful pushes"
      [PushInt 8, PushInt 9, PushInt 1, PushInt 2, PushInt 3, PushInt 3, PushInt 2, Roll]
      [PushInt 8, PushInt 9, PushInt 2, PushInt 3, PushInt 1]
  , run
      "reorganize stack with addresses"
      [InChar, Duplicate, PushInt 1, PushAddr someAddress, PushInt 3, PushInt 3, PushInt 2, Roll]
      [InChar, Duplicate, PushAddr someAddress, PushInt 3, PushInt 1]
  ]
  where
    someAddress = (Name (BaseName ["Main"] "foo") [], Label 1 2)

test = testGroup "remove redundant jumps (return)"
  [ run
      "remove simple jump"
      [Entrance mainFoo00, PushAddr mainFoo12, Multiply, PushAddr mainFoo12, Return, Entrance mainFoo12]
      [Entrance mainFoo00, PushAddr mainFoo12, Multiply, Entrance mainFoo12]
  , run
      "remove simple jumps"
      [Entrance mainFoo00, PushAddr mainFoo12, PushAddr mainFoo22, Multiply, PushAddr mainFoo12, Return, Entrance mainFoo12, Multiply, PushAddr mainFoo22, Return, Entrance mainFoo22]
      [Entrance mainFoo00, PushAddr mainFoo12, PushAddr mainFoo22, Multiply, Entrance mainFoo12, Multiply, Entrance mainFoo22]
  , run
      "do not remove valid jumps"
      [Entrance mainFoo00, PushAddr mainFoo12, Multiply, PushAddr mainFoo22, Return, Entrance mainFoo12]
      [Entrance mainFoo00, PushAddr mainFoo12, Multiply, PushAddr mainFoo22, Return, Entrance mainFoo12]
  ]
  where
    mainFoo00 = (Name (BaseName ["Main"] "foo") [], Label 0 0)
    mainFoo12 = (Name (BaseName ["Main"] "foo") [], Label 1 2)
    mainFoo22 = (Name (BaseName ["Main"] "foo") [], Label 2 2)

test = testGroup "remove redundant jumps (branch)"
  [ run
      "remove simple jump"
      [ Entrance mainFoo00, PushAddr mainFoo12, Multiply
      , InChar, PushAddr mainFoo12, PushInt 2, PushInt 1, Roll, Branch, Pop, Entrance mainFoo12
      ]
      [ Entrance mainFoo00, PushAddr mainFoo12, Multiply
      , InChar, Pop, Entrance mainFoo12
      ]
  , run
      "remove simple jumps"
      [ Entrance mainFoo00, PushAddr mainFoo12, PushAddr mainFoo22, Multiply
      , InInt, PushAddr mainFoo12, PushInt 2, PushInt 1, Roll, Branch, Pop, Entrance mainFoo12
      , InInt, PushAddr mainFoo22, PushInt 2, PushInt 1, Roll, Branch, Pop, Entrance mainFoo22
      ]
      [ Entrance mainFoo00, PushAddr mainFoo12, PushAddr mainFoo22, Multiply
      , InInt, Pop, Entrance mainFoo12
      , InInt, Pop, Entrance mainFoo22
      ]
  , run
      "do not remove valid jumps"
      [ Entrance mainFoo00, PushAddr mainFoo12, Multiply
      , PushAddr mainFoo22, PushInt 2, PushInt 1, Roll, Branch, Pop, Entrance mainFoo12
      ]
      [ Entrance mainFoo00, PushAddr mainFoo12, Multiply
      , PushAddr mainFoo22, PushInt 2, PushInt 1, Roll, Branch, Pop, Entrance mainFoo12
      ]
  ]
  where
    mainFoo00 = (Name (BaseName ["Main"] "foo") [], Label 0 0)
    mainFoo12 = (Name (BaseName ["Main"] "foo") [], Label 1 2)
    mainFoo22 = (Name (BaseName ["Main"] "foo") [], Label 2 2)

test = testGroup "remove push pop"
  [ run
      "remove push pop"
      [PushInt 1, Pop]
      []
  , run
      "remove push pop (x2)"
      [PushInt 1, Pop, PushInt 2, Pop]
      []
  , run
      "remove nested push pop"
      [PushInt 1, PushInt 2, Pop, Pop]
      []
  , run
      "remove duplicate pop"
      [PushInt 1, Duplicate, PushInt 2, Duplicate, Pop, Pop, Pop, Pop]
      []
  , run
      "do not remove valid push pops"
      [PushInt 1, PushInt 2, PushAddr mainFoo12, Return, Pop, Pop]
      [PushInt 1, PushInt 2, PushAddr mainFoo12, Return, Pop, Pop]
  ]
  where
    mainFoo12 = (Name (BaseName ["Main"] "foo") [], Label 1 2)

test = testGroup "deduplicate push"
  [ run
      "deduplicate simple push (int)"
      [PushInt 32424, PushInt 32424]
      [PushInt 32424, Duplicate]
  , run
      "deduplicate simple push (addr)"
      [PushAddr mainFoo12, PushAddr mainFoo12]
      [PushAddr mainFoo12, Duplicate]
  , run
      "deduplicate multiple pushes"
      [PushInt 32424, PushInt 32424, PushInt 32424, PushInt 32424, PushAddr mainFoo12, PushAddr mainFoo12, PushAddr mainFoo12]
      [PushInt 32424, Duplicate, Duplicate, Duplicate, PushAddr mainFoo12, Duplicate, Duplicate]
  , run
      "do not deduplicate different pushes"
      [PushInt 32424, PushInt 32425, PushInt 32424]
      [PushInt 32424, PushInt 32425, PushInt 32424]
  ]
  where
    mainFoo12 = (Name (BaseName ["Main"] "foo") [], Label 1 2)

test = testGroup "remove unused entrances"
  [ run
      "do not remove first entrance"
      [Entrance mainFoo00, PushInt 4, Multiply, Return]
      [Entrance mainFoo00, PushInt 4, Multiply, Return]
  , run
      "do not remove a used entrance (jump before)"
      [Entrance mainFoo00, PushAddr mainFoo12, Roll, Branch, Multiply, Entrance mainFoo12, Return]
      [Entrance mainFoo00, PushAddr mainFoo12, Roll, Branch, Multiply, Entrance mainFoo12, Return]
  , run
      "do not remove a used entrance (jump after)"
      [Entrance mainFoo00, Entrance mainFoo12, Return, PushAddr mainFoo12, Return]
      [Entrance mainFoo00, Entrance mainFoo12, Return, PushAddr mainFoo12, Return]
  ]
  where
    mainFoo00 = (Name (BaseName ["Main"] "foo") [], Label 0 0)
    mainFoo12 = (Name (BaseName ["Main"] "foo") [], Label 1 2)

test = testGroup "combined"
  [ run
      "merge > remove"
      [PushInt 3, PushInt 1, Roll, PushInt 3, PushInt 2, Roll]
      []
  , run
      "reorganize + remove branch + push pop + unused entrance"
      [PushInt 5, PushInt 4, PushInt 2, PushInt 1, Roll, PushAddr mainFoo12, PushInt 2, PushInt 1, Roll, Branch, Pop, Entrance mainFoo12, Pop]
      []
  ]
  where
    mainFoo12 = (Name (BaseName ["Main"] "foo") [], Label 1 2)


run
  :: String
  -> InstructionBuffer
  -> InstructionBuffer
  -> TestTree
run name input output =
  testCase name $ minimize input @?= output
