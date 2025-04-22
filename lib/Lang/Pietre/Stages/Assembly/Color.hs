module Lang.Pietre.Stages.Assembly.Color where

import "this" Prelude

import Lang.Pietre.Representations.Bytecode


data Color
  = LightRed     | Red     | DarkRed
  | LightYellow  | Yellow  | DarkYellow
  | LightGreen   | Green   | DarkGreen
  | LightCyan    | Cyan    | DarkCyan
  | LightBlue    | Blue    | DarkBlue
  | LightMagenta | Magenta | DarkMagenta
  | Black
  | White
  deriving (Show, Eq, Ord, Enum, Bounded)


step :: Int -> Int -> Color -> Color
step diffHue diffLight color = toEnum $
  3 * mod (startHue + diffHue) 6 + mod (startLight + diffLight) 3
  where
    (startHue, startLight) = fromEnum color `divMod` 3

nextColor :: Color -> Instruction a -> Color
nextColor color = \case
  PushInt _  -> step 0 1 color
  PushAddr _ -> step 0 1 color
  Pop        -> step 0 2 color
  Add        -> step 1 0 color
  Subtract   -> step 1 1 color
  Multiply   -> step 1 2 color
  Divide     -> step 2 0 color
  Mod        -> step 2 1 color
  Not        -> step 2 2 color
  Greater    -> step 3 0 color
  Duplicate  -> step 4 0 color
  Roll       -> step 4 1 color
  InInt      -> step 4 2 color
  InChar     -> step 5 0 color
  OutInt     -> step 5 1 color
  OutChar    -> step 5 2 color
  Entrance _ -> color
  Branch     -> color
  Return     -> color
  Terminate  -> Red
