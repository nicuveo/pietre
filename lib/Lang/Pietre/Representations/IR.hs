module Lang.Pietre.Representations.IR where

import "this" Prelude

import Lang.Pietre.Representations.Name


type IR = HashMap Name Function

data Function = Function
  { _funcStart  :: Block
  , _funcBlocks :: HashMap Label Block
  , _funcArgs   :: [Register]
  , _funcReturn :: [Register]
  } deriving Show

data Block = Block
  { _blockArguments    :: [Register]
  , _blockInstructions :: [Instruction]
  , _blockTerminator   :: Terminator
  } deriving Show

newtype Label = Label Int
  deriving Show

data Register = Register
  { _registerIndex :: Int
  , _registerType  :: Type
  } deriving Show

data Type
  = IntType
  | BoolType
  | CharType
  | EnumType Int
  | StructType [Type]
  | FunctionType [Type] [Type]
  deriving Show

data Terminator
  = Jump   Target
  | Branch Target Target Register
  | Return (Maybe Register)
  | Panic
  deriving Show

data Target = Target Label [Register]
  deriving Show

data Instruction
  = Add      Register Register Register
  | Subtract Register Register Register
  | Multiply Register Register Register
  | Divide   Register Register Register
  | Modulo   Register Register Register
  | Exponent Register Register Register
  | CmpEQ    Register Register Register
  | CmpNE    Register Register Register
  | CmpLT    Register Register Register
  | CmpLE    Register Register Register
  | CmpGT    Register Register Register
  | CmpGE    Register Register Register
  | NegateI  Register Register
  | NegateB  Register Register
  | And      Register Register
  | Or       Register Register
  | Cast     Register Register Type
  | AssignI  Register Int
  | AssignB  Register Bool
  | AssignC  Register Char
  | Combine  Register [Register]
  | GetField Register Register Int
  | SetField Register Register Int Register
  | InvokeN  [Register] Name     [Register]
  | InvokeR  [Register] Register [Register]
  deriving Show
