{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.IR where

import "this" Prelude

import Control.Lens.TH
import Data.Hashable
import Lang.Pietre.Representations.Name


type IR = HashMap Name Function

data Type
  = IntType
  | BoolType
  | CharType
  | EnumType Int
  | StructType BaseName [Type]
  | FunctionType [Type] (Maybe Type)

data Function = Function
  { _funBlocks :: NonEmpty (Label, Block)
  }
  deriving Show

data Block = Block
  { _blockParents      :: [Label]
  , _blockArguments    :: [Register]
  , _blockInstructions :: [Instruction]
  , _blockTerminator   :: Terminator
  } deriving Show

data Label = Label
  { _labelBlock :: Int
  , _labelInner :: Int
  }
  deriving (Show, Eq)

instance Hashable Label where
  hashWithSalt s (Label b i) = s `hashWithSalt` b `hashWithSalt` i

data Register = Register
  { _registerIndex :: Int
  , _registerType  :: Type
  }

instance Eq Register where
  (==) = (==) `on` _registerIndex

instance Ord Register where
  compare = compare `on` _registerIndex

instance Hashable Register where
  hashWithSalt s Register{..} = hashWithSalt s _registerIndex


-- TMP TMP TMP

instance Show Register where
  show Register {..} =
    let typePrefix = case _registerType of
          IntType          -> "i"
          BoolType         -> "b"
          CharType         -> "c"
          EnumType _       -> "e"
          StructType _ _   -> "s"
          FunctionType _ _ -> "f"
    in typePrefix ++ show _registerIndex

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
  | Exponent Register Register Register -- TODO: remove this
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
  | AssignA  Register Name
  | Combine  Register (NonEmpty Register)
  | GetField Register Register Int
  | SetField Register Register Int Register
  | InvokeN  (Maybe Register) Name     [Register]
  | InvokeR  (Maybe Register) Register [Register]
  deriving Show

makeLenses ''Function
makeLenses ''Block
makeLenses ''Register
makeLenses ''Target
makePrisms ''Terminator
makePrisms ''Target
makePrisms ''Instruction
