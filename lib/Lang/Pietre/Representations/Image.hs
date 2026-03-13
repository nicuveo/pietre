module Lang.Pietre.Representations.Image where

import Data.Word      (Word8)
import Graphics.Image qualified as I


type Image = I.Image I.VS I.RGBA Word8
