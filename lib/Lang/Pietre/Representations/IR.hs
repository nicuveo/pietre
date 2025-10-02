{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.IR where

import "this" Prelude

import Control.Lens.TH
import Lang.Pietre.Representations.Name


type IR = HashMap Name Function

data Function = Function
  { _funStart  :: Label
  , _funBlocks :: NonEmpty (Label, Block)
  }
  deriving Show

data Block = Block
  { _blockParents      :: [Label]
  , _blockArguments    :: [Register]
  , _blockInstructions :: [Instruction]
  , _blockTerminator   :: Terminator
  } deriving Show

newtype Label = Label Int
  deriving (Show, Eq, Hashable)

data Register = Register
  { _registerIndex :: Int
  , _registerType  :: Type
  } -- deriving Show

-- TMP TMP TMP

instance Show Register where
  show Register {..} =
    let typePrefix = case _registerType of
          IntType  -> "i"
          BoolType -> "b"
          CharType -> "c"
          _        -> undefined
    in typePrefix ++ show _registerIndex


data Type
  = IntType
  | BoolType
  | CharType
  | EnumType Int
  | StructType [Type]
  | FunctionType [Type] [Type]
  deriving (Show, Eq)

data Terminator
  = Jump   Target
  | Branch Target Target Register
  | Return (Maybe Register)
  | Panic
  deriving Show

data Target = Target
  { _tgtLabel :: Label
  , _tgtArgs  :: [Register]
  }
  deriving Show

mkTarget :: Label -> Target
mkTarget label = Target label []

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
  | Cast     Register Register
  | AssignI  Register Int
  | AssignB  Register Bool
  | AssignC  Register Char
  | Combine  Register [Register]
  | GetField Register Register Int
  | SetField Register Register Int Register
  | InvokeN  (Maybe Register) Name     [Register]
  | InvokeR  (Maybe Register) Register [Register]
  deriving Show

makeLenses ''Function
makeLenses ''Block
makeLenses ''Register
makeLenses ''Target
makePrisms ''Type
makePrisms ''Terminator
makePrisms ''Target
makePrisms ''Instruction
