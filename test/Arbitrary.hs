{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import "this" Prelude

import Data.String           (fromString)
import Test.Tasty.QuickCheck


-- source file

newtype SourceFile = SourceFile Text
  deriving (Show, Eq)

instance Arbitrary SourceFile where
  arbitrary = do
    content <- arbitrary
    pure $ SourceFile $ fromString $ getPrintableString content
