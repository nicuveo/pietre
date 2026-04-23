{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lang.Pietre.Stages.Assembly.Templates where

import "this" Prelude

import Data.ByteString.Char8                      (ByteString)
import Data.ByteString.Char8                      qualified as B
import Data.Vector                                qualified as V
import Graphics.Image                             qualified as I

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Image
import Lang.Pietre.Stages.Assembly.Color
import Lang.Pietre.Stages.Assembly.Templates.Load


data Template = Template
  { templateRows    :: Int
  , templateColumns :: Int
  , templateData    :: V.Vector TemplatePixel
  }
  deriving (Show)

data TemplatePixel
  = Absolute Color
  | Relative Int Int
  | Start
  | Transparent
  deriving (Show)


applyTemplate
  :: (Int, Int)
  -> Image
  -> Image
  -> Image
applyTemplate (!i0, !j0) !imgA !imgB = I.traverse2 imgB imgA const newPx
  where
    (!m, !n) = I.dims imgA
    newPx getPxB getPxA (i, j) =
      let !(i', j') = (i - i0, j - j0)
      in
        if i' >= 0 && j' >= 0 && i' < m && j' < n
        then
          let pixA@(I.PixelRGBA _ _ _ a1) = getPxA (i', j')
          in
            if a1 == 0 then
              getPxB (i, j)
            else
              pixA
        else getPxB (i, j)

readTemplate :: ByteString -> Template
readTemplate s = Template (length rows) (maximum $ map length rows)
  $ V.fromList
  $ map translate
  $ concat rows
  where
    rows = lines $ B.unpack s
    translate = \case
      ' ' -> Transparent
      '#' -> Absolute Black
      '.' -> Absolute White
      '0' -> Start
      'P' -> Relative 0 1
      'X' -> Relative 0 2
      '+' -> Relative 1 0
      '-' -> Relative 1 1
      '*' -> Relative 1 2
      '/' -> Relative 2 0
      '%' -> Relative 2 1
      '!' -> Relative 2 2
      '>' -> Relative 3 0
      '^' -> Relative 3 1
      'S' -> Relative 3 2
      'D' -> Relative 4 0
      'R' -> Relative 4 1
      'i' -> Relative 4 2
      'c' -> Relative 5 0
      'I' -> Relative 5 1
      'C' -> Relative 5 2
      c   -> reportICE "readTemplate" ("unknown character: '" ++ [c] ++ "'") []

makeImage :: Color -> Template -> Image
makeImage startingColor Template{..} =
  I.makeImage (templateRows, templateColumns) \(r, c) ->
    case templateData V.! (templateColumns * r + c) of
      Transparent        -> I.PixelRGBA 0 0 0 0
      Absolute color     -> convert color
      Relative hue light -> convert $ step hue light startingColor
      Start              -> convert startingColor
  where
    convert = \case
     LightRed     -> I.PixelRGBA 0xFF 0xC0 0xC0 0xFF
     Red          -> I.PixelRGBA 0xFF 0x00 0x00 0xFF
     DarkRed      -> I.PixelRGBA 0xC0 0x00 0x00 0xFF
     LightYellow  -> I.PixelRGBA 0xFF 0xFF 0xC0 0xFF
     Yellow       -> I.PixelRGBA 0xFF 0xFF 0x00 0xFF
     DarkYellow   -> I.PixelRGBA 0xC0 0xC0 0x00 0xFF
     LightGreen   -> I.PixelRGBA 0xC0 0xFF 0xC0 0xFF
     Green        -> I.PixelRGBA 0x00 0xFF 0x00 0xFF
     DarkGreen    -> I.PixelRGBA 0x00 0xC0 0x00 0xFF
     LightCyan    -> I.PixelRGBA 0xC0 0xFF 0xFF 0xFF
     Cyan         -> I.PixelRGBA 0x00 0xFF 0xFF 0xFF
     DarkCyan     -> I.PixelRGBA 0x00 0xC0 0xC0 0xFF
     LightBlue    -> I.PixelRGBA 0xC0 0xC0 0xFF 0xFF
     Blue         -> I.PixelRGBA 0x00 0x00 0xFF 0xFF
     DarkBlue     -> I.PixelRGBA 0x00 0x00 0xC0 0xFF
     LightMagenta -> I.PixelRGBA 0xFF 0xC0 0xFF 0xFF
     Magenta      -> I.PixelRGBA 0xFF 0x00 0xFF 0xFF
     DarkMagenta  -> I.PixelRGBA 0xC0 0x00 0xC0 0xFF
     Black        -> I.PixelRGBA 0x00 0x00 0x00 0xFF
     White        -> I.PixelRGBA 0xFF 0xFF 0xFF 0xFF


mkTemplate :: ByteString -> Color -> Image
mkTemplate template color = makeImage color $ readTemplate template

$(generateTemplates 'mkTemplate)

push4Template = instructionTemplate
