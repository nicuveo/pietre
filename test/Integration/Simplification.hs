{- AUTOCOLLECT.TEST -}
{-# LANGUAGE TemplateHaskell #-}

module Integration.Simplification
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import "this" Prelude

import Data.HashMap.Strict                  qualified as M
import Data.HashSet                         qualified as S
import Data.Text.IO                         qualified as T
import System.FilePath
import Test.Tasty.HUnit

import Lang.Pietre
import Lang.Pietre.Representations.Location

import Compile
import Locate


expressions :: Definition Resolved -> [TypedExpression]
expressions = \case
  ConstDef    ConstInfo    {..} -> pure _constExpr
  FunctionDef FunctionInfo {..} -> fromBlock _funBody
  _ -> []
  where
    fromBlock = concatMap fromStatement
    fromStatement = \case
      IfStmt         ifInfo    -> fromIf ifInfo
      ForStmt        forInfo   -> fromFor forInfo
      WhileStmt      whileInfo -> fromWhile whileInfo
      LetStmt        letInfo   -> pure $ _letExpr letInfo
      ReturnStmt     expr      -> maybeToList expr
      ExpressionStmt expr      -> pure expr
      _ -> []
    fromIf IfInfo {..} =
      _ifExpr : fromBlock _ifBody <> foldMap fromElse _ifElse
    fromElse = \case
      ElseIf ifInfo   -> fromIf ifInfo
      ElseBlock block -> fromBlock block
    fromFor ForInfo {..} =
      _forRangeExpr : fromBlock _forBody
    fromWhile WhileInfo {..} =
      _whileExpr : fromBlock _whileBody


test_batch = do
  sourceFile <- $(listFiles "test/Integration/simplification" ".source")
  let
    testName = takeBaseName sourceFile
    simplifiedFile = sourceFile -<.> ".simplified"
    fakeFilename = testName <> ".pi"
  pure $ testCase testName do
    source     <- T.readFile sourceFile
    simplified <- T.readFile simplifiedFile
    reference  <- runTestCompiler fakeFilename (parse simplified >>= analyze)
      `onLeftM` assertFailure
    testValue  <- runTestCompiler fakeFilename (parse source     >>= analyze >>= simplify)
      `onLeftM` assertFailure
    let testDefinitions = _resmodDefinitions testValue
        refDefinitions  = _resmodDefinitions reference
        allNames        = S.union (M.keysSet testDefinitions) (M.keysSet refDefinitions)
    for_ allNames \name -> do
      let refExprs  = expressions $ _located $ refDefinitions  M.! name
          testExprs = expressions $ _located $ testDefinitions M.! name
      sequence $ zipWith (@?=) testExprs refExprs
