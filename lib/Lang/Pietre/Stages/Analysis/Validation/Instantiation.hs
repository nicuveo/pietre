module Lang.Pietre.Stages.Analysis.Validation.Instantiation where

import "this" Prelude


import Control.Lens                                 hiding (mapping, op)
import Control.Monad.Loops                          (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                    (hoistMaybe)
import Data.HashMap.Strict.Extra                    qualified as M
import Data.List                                    qualified as L
import Data.Ordered.Set                             qualified as OSet
import Data.Seq                                     qualified as Seq

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Resolved     as Resolved
import Lang.Pietre.Representations.AST.Validated    as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Expr
import Lang.Pietre.Stages.Analysis.Validation.Monad


instantiateAllSymbols
  :: Monad m
  => ValidateT m SymbolCache
instantiateAllSymbols = do
  originalRequests <- uses vsInstanceRequests
  M.catMaybes . snd <$> iterateUntilM (Seq.null . fst) processBatch (originalRequests, M.empty)
  where
    processBatch (requests, cache) = do
      vsInstanceRequests .= Seq.empty
      newSymbols <- M.fromList . L.catMaybes <$> traverse (processRequest cache) requests
      newRequests <- uses vsInstanceRequests
      pure (newRequests, M.union cache newSymbols)

    processRequest cache request@FunctionInstantiationRequest {..} = do
      let name = Name _firBaseName $ map assertName _firParams
      if M.member name cache
      then pure Nothing
      else do
        symbol <- asumM
          [ views viSymbols $ M.lookup name
          , try $ withContext baseName (_location definition) (instantiate request)
          ]
        pure (name, symbol)

instantiate
  :: Monad m
  => FunctionInstantiationRequest
  -> ValidateT m Validated.FunctionInfo
instantiate FunctionInstantiationRequest {..} = do
  let Validated.FunctionType {..} = _firFunType
  baseName <- currentName
  currentParams  .= M.fromList (zip (map (baseName,) _funParams) _firParams)
  currentFunType .= _funReturn
  functionBody <- validateBlock $ Common._funBody _firDefinition
  pure $ Validated.FunctionInfo _firFunType functionBody


validateBlock
  :: Monad m
  => ValidateT m ()
  -> [WithLocation (Statement Resolved)]
  -> ValidateT m [Statement Validated]
validateBlock initContext stmts = do
  variables <- use currentVariables
  initContext
  attemptedStatements <- traverse (tryNested . validatedStatement) stmts
  currentVariables .= variables
  ensure attemptedStatements

validateStatement
  :: Monad m
  => WithLocation (Statement Resolved)
  -> ValidateT m (Statement Validated)
validateStatement statement = do
  currentLocation .= _location statement
  case _located statement of
    ContinueStmt ->
      pure ContinueStmt
    BreakStmt -> do
      pure BreakStmt
    ReturnStmt returnExpr ->
      validateReturnStatement returnExpr
    ExpressionStmt expr ->
      validateExpressionStatement expr
    LetStmt letInfo ->
      validateLetStatement letInfo
    IfStmt ifInfo ->
      validateIfStatement ifInfo
    WhileStmt whileInfo ->
      validateWhileStatement whileInfo
    ForStmt forInfo ->
      validateForStatement forInfo

validateReturnStatement
  :: Monad m
  => Maybe Resolved.Expression
  -> ValidateT m Validated.Statement
validateReturnStatement returnExpr = do
  validatedExpr <- traverse validateFunctionExpression returnExpr
  let exprReturnType = maybe UnitType _typeInfo validatedExpr
  funReturnType <- use currentFunType
  expectType funReturnType exprReturnType
  pure $ ReturnStmt validatedExpr

validateExpressionStatement
  :: Monad m
  => Resolved.Expression
  -> ValidateT m Validated.Statement
validateExpressionStatement resolvedExpr = do
  validatedExpr <- validateFunctionExpression resolvedExpr
  -- TODO: can this be simplified using gathered purity information?
  unless (_typeInfo validatedExpr `typeMatches` UnitType) $
    case _exprValue resolvedExpr of
      CallExpr _ _ -> pure ()
      e            -> warn $ WarningUnexpectedTopLevelExpression e
  pure $ ExpressionStmt resolvedExpr

validateLetStatement
  :: Monad m
  => Resolved.LetInfo
  -> ValidateT m Validated.Statement
validateLetStatement Common.LetInfo {..} = do
  attemptedExpr <- try $ validateFunctionExpression _letExpr
  for _letType \resolvedType -> do
    validatedPartialType <- validatePartialType resolvedType
    validateTypePattern validatedPartialType (_typeInfo validatedExpr)
  validatedExpr <- ensure attemptedExpr
  currentVariables %= M.insert _letName (_typeInfo validatedExpr)
  pure $ LetStmt $ LetInfo _letName validatedExpr

validateIfStatement
  :: Monad m
  => IfInfo Resolved
  -> ValidateT m Validated.Statement
validateIfStatement = fmap IfStmt . go
  where
    go IfInfo {..} = do
      validatedExpr <- try $ validateCondition _ifExpr
      validatedBody <- try $ validateBlock pass _ifBody
      validatedElse <- try $ traverse validateElse _ifElse
      ensure $ liftA3 IfInfo validatedExpr validatedBody validatedElse

    validateElse = \case
      ElseIf    ifInfo -> ElseIf    <$> go ifInfo
      ElseBlock block  -> ElseBlock <$> validateBlock pass block

validateWhileStatement
  :: Monad m
  => WhileInfo Resolved
  -> ValidateT m Validated.Statement
validateWhileStatement WhileInfo {..} = do
  validatedExpr <- try $ validatedExpr _whileExpr
  validatedBody <- try $ validateBlock pass _whileBody
  ensure $ WhileStmt <$> liftA2 WhileInfo validatedExpr validatedBody

validateCondition
  :: Monad m
  => Resolved.Expression
  -> ValidateT m Validated.Expression
validateCondition conditionExpr = do
  validatedExpr <- validateFunctionExpression _ifExpr
  expectType BoolType $ _typeInfo validatedExpr
  pure validatedExpr

validateForStatement
  :: Monad m
  => ForInfo Resolved
  -> ValidateT m Validated.Statement
validateForStatement ForInfo {..} = do
  (validatedRangeType, validatedRangeExpression) <- validateRangeExpression _forRangeExpr
  let setForVariable = currentVariables %= M.insert _forVariableName validatedRangeType
  validatedBody <- validateBlock setForVariable _forBody
  pure $ ForStmt $
    ForInfo
      _forVariableName
      validatedRangeType
      validatedRangeExpression
      validatedBody
