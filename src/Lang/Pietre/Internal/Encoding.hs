module Lang.Pietre.Internal.Encoding where

import "this" Prelude

import Data.Bits
import Data.Word      (Word8)


-- | Decompose a Char into a sequence of bytes.
--
-- This function is taken almost unchanged from Alex, and corresponds to what it
-- calls "utf8Encode'". A significant change we make here is to use 'NonEmpty'
-- instead of a tuple with a list.
decomposeUTF8 :: Char -> NonEmpty Word8
decomposeUTF8 c = fmap fromIntegral $
  if | s00 <= 0x7f   -> s00 :| []
     | s00 <= 0x7ff  -> (0xc0 + s06) :| [r00]
     | s00 <= 0xffff -> (0xe0 + s12) :| [r06, r00]
     | otherwise     -> (0xf0 + s18) :| [r12, r06, r00]
  where
    s00 = ord c
    s06 = s00 .>>.  6
    s12 = s00 .>>. 12
    s18 = s00 .>>. 18
    r00 = 0x80 + s00 .&. 0x3f
    r06 = 0x80 + s06 .&. 0x3f
    r12 = 0x80 + s12 .&. 0x3f
