module Lang.Pietre.Stages.Analysis.Validation.Instantiation (instantiateAllSymbols) where

import "this" Prelude

import Control.Lens                                  hiding (mapping, op)
import Control.Monad.Catch                           (bracket)
import Control.Monad.Loops                           (iterateUntilM)
import Data.HashMap.Strict.Extra                     qualified as M
import Data.Sequence                                 qualified as Seq

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Resolved      as Resolved
import Lang.Pietre.Representations.AST.Validated     as Validated
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Expect
import Lang.Pietre.Stages.Analysis.Validation.Expr
import Lang.Pietre.Stages.Analysis.Validation.Monad
import Lang.Pietre.Stages.Analysis.Validation.Types


--------------------------------------------------------------------------------
-- API

instantiateAllSymbols
  :: MonadDiagnosis m
  => ValidateT m SymbolCache
instantiateAllSymbols = do
  originalRequests <- use vsInstanceRequests
  snd <$> iterateUntilM (Seq.null . fst) processBatch (originalRequests, M.empty)
  where
    processBatch (requests, cache) = do
      vsInstanceRequests .= Seq.empty
      newSymbols <- M.fromList . catMaybes <$> traverse (processRequest cache) (toList requests)
      newRequests <- use vsInstanceRequests
      pure (newRequests, M.union cache newSymbols)

    processRequest cache request@FunctionInstantiationRequest {..} = do
      let name = Name _firBaseName $ map assertName _firParams
      if M.member name cache
      then pure Nothing
      else do
        symbol <- asumM
          [ views viSymbols $ M.lookup name
          , try
             $ withContext _firBaseName (_location _firDefinition)
             $ instantiate request
          ]
        pure $ sequenceA (name, symbol)


--------------------------------------------------------------------------------
-- Implementation

instantiate
  :: MonadDiagnosis m
  => FunctionInstantiationRequest
  -> ValidateT m Validated.FunctionInfo
instantiate FunctionInstantiationRequest {..} = do
  let FunctionTypeInfo {..} = _firFunType
  baseName <- use currentName
  currentParams  .= M.fromList (zip (map (baseName,) _funParams) _firParams)
  currentFunType .= _funReturn
  functionBody <- validateBlock pass $ Resolved._funBody $ _located _firDefinition
  pure $ Validated.FunctionInfo _firFunType functionBody

validateBlock
  :: MonadDiagnosis m
  => ValidateT m ()
  -> [WithLocation Resolved.Statement]
  -> ValidateT m [WithLocation Validated.Statement]
validateBlock initContext stmts =
  bracket
    initBlockVariables
    restoreVariables
    processBlock
  where
    initBlockVariables =
      use currentVariables
    restoreVariables variables =
      currentVariables .= variables
    processBlock _ = do
      initContext
      ensureNested $ traverse (tryNested . validateStatement) stmts

validateStatement
  :: MonadDiagnosis m
  => WithLocation Resolved.Statement
  -> ValidateT m (WithLocation Validated.Statement)
validateStatement statement = do
  currentLocation .= _location statement
  resultStatement <- case _located statement of
    ContinueStmt ->
      pure ContinueStmt
    BreakStmt -> do
      pure BreakStmt
    ReturnStmt returnExpr ->
      ReturnStmt <$> validateReturnStatement returnExpr
    ExpressionStmt expr ->
      ExpressionStmt <$> validateExpressionStatement expr
    LetStmt letInfo ->
      LetStmt <$> validateLetStatement letInfo
    IfStmt ifInfo ->
      IfStmt <$> validateIfStatement ifInfo
    WhileStmt whileInfo ->
      WhileStmt <$> validateWhileStatement whileInfo
    ForStmt forInfo ->
      ForStmt <$> validateForStatement forInfo
  pure $ resultStatement <$ statement

validateReturnStatement
  :: MonadDiagnosis m
  => Maybe (WithLocation Resolved.Expression)
  -> ValidateT m (Maybe (Typed Validated.Expression))
validateReturnStatement returnExpr = do
  validatedExpr <- traverse validateFunctionExpression returnExpr
  let exprReturnType = maybe UnitType _typeInfo validatedExpr
  funReturnType <- use currentFunType
  expectType funReturnType exprReturnType
  pure validatedExpr

validateExpressionStatement
  :: MonadDiagnosis m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed Validated.Expression)
validateExpressionStatement resolvedExpr = do
  validatedExpr <- validateFunctionExpression resolvedExpr
  -- TODO: can this be simplified using gathered purity information?
  unless (_typeInfo validatedExpr `typeMatches` UnitType) $
    case _typedValue validatedExpr of
      FunctionCallExpr _ _ _ -> pass
      VariableCallExpr _ _ _ -> pass
      e                      -> warn $ WarningUnexpectedTopLevelExpression e
  pure validatedExpr

validateLetStatement
  :: MonadDiagnosis m
  => Resolved.LetInfo
  -> ValidateT m Validated.LetInfo
validateLetStatement Resolved.LetInfo {..} = do
  attemptedExpr <- try $ validateFunctionExpression _letExpr
  attemptedType <- try $ traverse validatePartialType _letType
  validatedExpr <- ensure attemptedExpr
  validatedType <- ensure attemptedType
  for_ validatedType \validatedPartialType ->
    validateTypePattern validatedPartialType (_typeInfo validatedExpr)
  currentVariables %= M.insert _letName (_typeInfo validatedExpr)
  pure $ Validated.LetInfo _letName validatedExpr

validateIfStatement
  :: MonadDiagnosis m
  => Resolved.IfInfo
  -> ValidateT m Validated.IfInfo
validateIfStatement = go
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
  :: MonadDiagnosis m
  => Resolved.WhileInfo
  -> ValidateT m Validated.WhileInfo
validateWhileStatement WhileInfo {..} = do
  validatedExpr <- try $ validateFunctionExpression _whileExpr
  validatedBody <- try $ validateBlock pass _whileBody
  ensure $ liftA2 WhileInfo validatedExpr validatedBody

validateCondition
  :: MonadDiagnosis m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed Validated.Expression)
validateCondition conditionExpr = do
  validatedExpr <- validateFunctionExpression conditionExpr
  expectType BoolType $ _typeInfo validatedExpr
  pure validatedExpr

validateForStatement
  :: MonadDiagnosis m
  => Resolved.ForInfo
  -> ValidateT m Validated.ForInfo
validateForStatement Resolved.ForInfo {..} = do
  (validatedRangeType, validatedRangeExpression) <- validateRangeExpression _forRangeExpr
  let setForVariable = currentVariables %= M.insert _forVariableName validatedRangeType
  validatedBody <- validateBlock setForVariable _forBody
  pure $ Validated.ForInfo
    _forVariableName
    validatedRangeType
    validatedRangeExpression
    validatedBody
