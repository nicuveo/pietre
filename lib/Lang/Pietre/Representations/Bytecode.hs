module Lang.Pietre.Representations.Bytecode where

import "this" Prelude

import Data.Kind                        (Type)

import Lang.Pietre.Representations.IR   qualified as IR
import Lang.Pietre.Representations.Name


data LinkerPhase = Unresolved | Resolved

class
  ( Show (PushAddress     p)
  , Show (EntranceAddress p)
  , Eq   (PushAddress     p)
  , Eq   (EntranceAddress p)
  ) => PhaseTypes (p :: LinkerPhase) where
  type PushAddress     p :: Type
  type EntranceAddress p :: Type

type Address = (Name, IR.Label)

instance PhaseTypes Unresolved where
  type PushAddress     Unresolved = Address
  type EntranceAddress Unresolved = Address

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
deriving instance PhaseTypes p => Eq   (Instruction p)


type InstructionBuffer = Seq (Instruction Unresolved)

type Object = HashMap Name InstructionBuffer
