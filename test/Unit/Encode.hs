{- AUTOCOLLECT.TEST -}

{-# LANGUAGE OverloadedLists #-}

module Unit.Encode
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.Word                     (Word8)
import Lang.Pietre.Internal.Encoding
import Test.Tasty
import Test.Tasty.HUnit


check :: Char -> NonEmpty Word8 -> TestTree
check c expected = testCase [c] $ decomposeUTF8 c @?= expected


test = testGroup "simple ascii values" do
  c <- "a#~.0W"
  pure $ check c $ pure $ fromIntegral $ ord c

test = testGroup "kanji" $ fmap (uncurry check)
  [ ('名', [0xe5, 0x90, 0x8d])
  , ('前', [0xe5, 0x89, 0x8d])
  ]
