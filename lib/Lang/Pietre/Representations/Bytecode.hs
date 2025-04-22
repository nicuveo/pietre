module Lang.Pietre.Representations.Bytecode where

import "this" Prelude

import Data.Kind      (Type)


data LinkerPhase = Unresolved | Resolved

class
  ( Show (PushAddress     p)
  , Show (EntranceAddress p)
  ) => PhaseTypes (p :: LinkerPhase) where
  type PushAddress     p :: Type
  type EntranceAddress p :: Type

instance PhaseTypes Unresolved where
  type PushAddress     Unresolved = Text
  type EntranceAddress Unresolved = Text

instance PhaseTypes Resolved where
  type PushAddress     Resolved = Void
  type EntranceAddress Resolved = ()


data Instruction (phase :: LinkerPhase)
  = PushInt  Int
  | PushAddr ~(PushAddress phase)
  | Pop
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | Not
  | Greater
  | Duplicate
  | Roll
  | InInt
  | InChar
  | OutInt
  | OutChar
  | Entrance ~(EntranceAddress phase)
  | Branch
  | Return
  | Terminate

deriving instance PhaseTypes p => Show (Instruction p)
