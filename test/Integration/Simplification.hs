{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module Integration.Simplification
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.HashMap.Strict                             qualified as M
import Data.HashSet                                    qualified as S
import Data.Text.IO                                    qualified as T
import System.FilePath
import Test.Tasty.HUnit

import Lang.Pietre.Export.PrettyPrinting.AST.Validated
import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location

import Compile
import Locate


expressions :: FunctionInfo -> [Typed Expression]
expressions = fromBlock . _funBody
  where
    fromBlock = concatMap (fromStatement . _located)
    fromStatement = \case
      IfStmt         ifInfo    -> fromIf ifInfo
      ForStmt        forInfo   -> fromFor forInfo
      WhileStmt      whileInfo -> fromWhile whileInfo
      LetStmt        letInfo   -> pure $ _letValue letInfo
      ReturnStmt     expr      -> maybeToList expr
      ExpressionStmt expr      -> pure expr
      _ -> []
    fromIf IfInfo {..} =
      _ifExpr : fromBlock _ifBody <> foldMap fromElse _ifElse
    fromElse = \case
      ElseIf ifInfo   -> fromIf ifInfo
      ElseBlock block -> fromBlock block
    fromFor ForInfo {..} =
      fromRangeExpression _forRangeExpr ++ fromBlock _forBody
    fromWhile WhileInfo {..} =
      _whileExpr : fromBlock _whileBody
    fromRangeExpression = \case
      RangeInclusiveExpr lhs rhs -> [lhs, rhs]
      RangeExclusiveExpr lhs rhs -> [lhs, rhs]



test_batch = do
  sourceFile <- $(listFiles "test/Integration/simplification" ".source")
  let
    testName = takeBaseName sourceFile
    simplifiedFile = sourceFile -<.> ".simplified"
    fakeFilename = testName <> ".pi"
  pure $ testCase testName do
    source     <- T.readFile sourceFile
    simplified <- T.readFile simplifiedFile
    let run action = snd (runTest fakeFilename mempty action) `onLeft` assertFailure
    reference  <- run (parse simplified >>= analyze)
    testValue  <- run (parse source     >>= analyze >>= simplify)
    let testSymbols = _interfaceSymbols testValue
        refSymbols  = _interfaceSymbols reference
        allNames    = S.union (M.keysSet testSymbols) (M.keysSet refSymbols)
    for_ allNames \name -> do
      let refExprs  = expressions $ refSymbols  M.! name
          testExprs = expressions $ testSymbols M.! name
      zipWithM ((@?=) `on` prettyPrintExpression) testExprs refExprs
