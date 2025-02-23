module Main where

{- AUTOCOLLECT.MAIN

group_type = tree
custom_main = True

-}

import "this" Prelude

import Test.Tasty

{- AUTOCOLLECT.MAIN.imports -}

main :: IO ()
main = defaultMain $ testGroup "pietre" {- AUTOCOLLECT.MAIN.tests -}
