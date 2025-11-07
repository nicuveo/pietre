module Lang.Pietre.Stages.Analysis.Validation (validate) where

import "this" Prelude

import Control.Lens                              hiding (mapping, op)
import Control.Monad.Loops                       (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                 (hoistMaybe)
import Data.HashMap.Strict.Extra                 qualified as M
import Data.HashSet                              qualified as S
import Data.Set                                  qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.AST.Common    as Input
import Lang.Pietre.Representations.AST.Resolved  as Input
import Lang.Pietre.Representations.AST.Validated as Output
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Core
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Instantiation
import Lang.Pietre.Stages.Analysis.Monad


--------------------------------------------------------------------------------
-- API

validate
  :: Monad m
  => Name
  -> WithLocation (Definition Resolved)
  -> Analyze m ()
validate name definition =
  withContext name (_location Definition) $
    case _located definition of
      TypeAliasDef _    -> pass
      StructDef    _    -> pass
      EnumDef      _    -> pass
      ConstDef     info -> void $ try $ validateConst info
      FunctionDef  info -> when (not $ isGeneric info) $ void $ try $ validateFunction info []

validateConst
  :: Monad m
  => ConstInfo Resolved
  -> Analyze m ()
validateConst ConstInfo {..} = do
  let constName = Name baseName []
  unlessM (uses vsValidated $ M.lookup constName) do
    vsValidated %= S.insert name
    validatedType <- validateType _constType
    validatedExpr <- validateConstExpression _constExpr
    unless (validatedType == _typeInfo resolvedExpr) $
      report $ ErrorWrongType [valdidatedType] (_typeInfo resolvedExpr)
    baseName <- currentName
    vsValueCache %= M.insert constName (Typed validatedType validateExpr)

validateConstExpr
  :: Monad m
  => WithLocation (Input.Expression Resolved)
  -> Analyze m (Typed Output.ConstExpression)
validateConstExpr WithLocation {..} = do
  currentLocation .= _location
  go _located
  where
    validateBinaryExprWith f e1 e2 = do
      lhs <- try $ validateConstExpr e1
      rhs <- try $ validateConstExpr e2
      ensure <$> liftA2 f lhs rhs
    go = \case
      Input.PathExpr path ->
        validateConstPathExpr path
      Input.CastExpr expr targetType -> do
        validatedExpr <- try $ validateConstExpr expr
        validatedType <- try $ validateType targetType
        ensure <$> liftA2 validateConstCastExpr validatedExpr validatedType
      Input.FieldAccessExpr expr field -> do
        validatedExpr <- validateConstExpr expr
        validateConstFieldAccessExpr validatedExpr field
      Input.CallExpr _ _ ->
        fatal unimplemented -- ErrorFunctionCallInConstExpression
      Input.ArrayExpr _ ->
        unimplemented
      Input.IndexExpr _ _ ->
        unimplemented
      Input.StructExpr structName fields -> do
        validateStructExpr validateConstExpr structName fields
      Input.IntLiteralExpr i ->
        pure $ Typed IntType $ Output.IntLiteralConstExpr i
      Input.BoolLiteralExpr b ->
        pure $ Typed BoolType $ Output.BoolLiteralConstExpr b
      Input.CharLiteralExpr c ->
        pure $ Typed CharType $ Output.CharLiteralConstExpr c
      Input.StringLiteralExpr s ->
        unimplemented
      Input.ReferenceExpr _ ->
        fatal unimplemented -- ErrorReferenceExpressionInConstExpression
      Input.BoolNegationExpr expr -> do
        validatedExpr <- validateConstExpr expr
        validateBoolNegationExpr validatedExpr
      Input.IntNegationExpr expr -> do
        validatedExpr <- validateConstExpr expr
        validateIntNegationExpr validatedExpr
      Input.AdditionExpr e1 e2 ->
        validateBinaryExprWith validateAdditionExpr e1 e2
      Input.SubtractionExpr e1 e2 ->
        validateBinaryExprWith validateSubtractionExpr e1 e2
      Input.MultiplicationExpr e1 e2 ->
        validateBinaryExprWith validateMultiplicationExpr e1 e2
      Input.ExponentiationExpr e1 e2 ->
        validateBinaryExprWith validateExponentiationExpr e1 e2
      Input.DivisionExpr e1 e2 ->
        validateBinaryExprWith validateDivisionExpr e1 e2
      Input.ModuloExpr e1 e2 ->
        validateBinaryExprWith validateModuloExpr e1 e2
      Input.EqualityExpr e1 e2 ->
        validateBinaryExprWith validateEqualityExpr e1 e2
      Input.DifferenceExpr e1 e2 ->
        validateBinaryExprWith validateDifferenceExpr e1 e2
      Input.GreaterExpr e1 e2 ->
        validateBinaryExprWith validateGreaterExpr e1 e2
      Input.LesserExpr e1 e2 ->
        validateBinaryExprWith validateLesserExpr e1 e2
      Input.GreaterEqExpr e1 e2 ->
        validateBinaryExprWith validateGreaterEqExpr e1 e2
      Input.LesserEqExpr e1 e2 ->
        validateBinaryExprWith validateLesserEqExpr e1 e2
      Input.BoolAndExpr e1 e2 ->
        validateBinaryExprWith validateBoolAndExpr e1 e2
      Input.BoolOrExpr e1 e2 ->
        validateBinaryExprWith validateBoolOrExpr e1 e2
      RangeInclusiveExpr _ _ ->
        unimplemented
      RangeExclusiveExpr _ _ ->
        unimplemented
      AssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion
      AdditionAssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion
      SubtractionAssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion
      MultiplicationAssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion
      DivisionAssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion
      ModuloAssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion
      ExponentiationAssignmentExpr _ _ ->
        fatal unimplemented -- ErrorAssignmentInConstExpresssion

validateConstPathExpr
  :: Monad m
  => PathInfo Resolved
  -> Validate m (Typed Output.ConstExpression)
validateConstPathExpr PathInfo {..} = do
  case _pathName of
    Constant name -> validateOtherConstValue name
    role          -> report $ ErrorNotAConst role
  where
    validateOtherConstValue name = do
      let actual = length _pathParams
      when (actual /= 0) $
        report $ ErrorIncorrectTypeParameterCount name 0 actual
      unimplemented -- validate recursively

validateConstCastExpr
  :: Monad m
  => Typed Output.ConstExpression
  -> Output.Type
  -> Validate m (Typed Output.ConstExpression)
validateConstCastExpr validatedExpr targetType =
  validateCastExpr validatedExpr targetType intResult charResult boolResult enumResult
  where
    intValue =
      case _typedValue validatedExpr of
        BoolLiteralConstExpr b -> fromEnum b
        IntLiteralConstExpr  i -> i
        CharLiteralConstExpr c -> ord c
        _ -> reportICE "const cast validation" "unexpected LHS" ["lhs: " ++ show validatedExpr]
    intResult =
      pure $ Output.IntLiteralConstExpr intValue
    charResult =
      pure $ Output.CharLiteralConstExpr $ chr intValue
    boolResult =
      pure $ Output.BoolLiteralConstExpr $ intValue /= 0
    enumResult name values =
      if intValue < 0 || intValue >= length values
      then report $ ErrorEnumOutOfBounds name intValue
      else pure $ Output.IntLiteralConstExpr intValue

validateFunctionCastExpr
  :: Monad m
  => Typed Output.Expression
  -> Output.Type
  -> Validate m (Typed Output.Expression)
validateFunctionCastExpr validatedExpr targetType =
  validateCastExpr validatedExpr targetType intResult charResult boolResult enumResult
  where
    resultWith f =
      case _typedValue validatedExpr of
        BoolLiteralExpr b -> f $ fromEnum b
        IntLiteralExpr  i -> f $ i
        CharLiteralExpr c -> f $ ord c
        _                 -> pure $ Output.CastExpr validatedExpr targetType
    intResult =
      resultWith (pure . Output.IntLiteralExpr)
    charResult =
      resultWith (pure . Output.CharLiteralExpr . chr)
    boolResult =
      resultWith (pure . Output.BoolLiteralExpr . (/= 0))
    enumResult name values =
      resultWith \v ->
        if v < 0 || v >= length values
        then report $ ErrorEnumOutOfBounds name intValue
        else pure $ Output.IntLiteralConstExpr intValue

validateCastExpr
  :: Monad m
  => Typed expr
  -> Output.Type
  -> Validate m (Typed expr)
  -> Validate m (Typed expr)
  -> Validate m (Typed expr)
  -> (Name -> NonEmpty (Identifier, Name) -> Validate m (Typed expr))
  -> Validate m (Typed expr)
validateCastExpr validatedExpr targetType intResult charResult boolResult enumResult = do
  innerValue <-
    case (_typeInfo validatedExpr, targetType) of
      (IntType, IntType) ->
        intResult
      (IntType, BoolType) ->
        boolResult
      (IntType, CharType) ->
        charResult
      (BoolType, IntType) ->
        intResult
      (BoolType, BoolType) ->
        boolResult
      (CharType, IntType) ->
        intResult
      (CharType, CharType) ->
        charResult
      (CharType, CharType) ->
        charResult
      (EnumType _ _, IntType) ->
        intResult
      (IntType, EnumType enumName enumValues) -> _
        enumResult name enumValues
      (EnumType _ _) (EnumType enumName enumValues) ->
        enumResult name enumValues
      _ ->
        fatal $ ErrorWrongCast (_typeInfo validatedExpr) targetType
  pure $ Typed targetType innerValue

validateConstFieldAccessExpr
  :: Monad m
  => Typed ConstExpression
  -> Identifier
  -> Validate m (Typed ConstExpression)
validateConstFieldAccessExpr validatedExpr fieldName = do
  case _typedValue validatedExpr of
    StructConstExpr _ _ fields -> do
      (_fieldName, fieldValue) <-
        find ((fieldName ==) . fst) fields `onNothing`
        fatal (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure fieldValue
    _ -> fatal $ ErrorFieldAccessNotAStruct (_typeInfo validatedExpr)

validateFunctionFieldAccessExpr
  :: Monad m
  => Typed ConstExpression
  -> Identifier
  -> Validate m (Typed ConstExpression)
validateFunctionFieldAccessExpr validatedExpr fieldName = do
  case _typedValue validatedExpr of
    StructConstExpr _ _ fields -> do
      (_fieldName, fieldValue) <-
        find ((fieldName ==) . fst) fields `onNothing`
        fatal (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure fieldValue
    _ -> unimplemented

validateFunction
  :: Monad m
  => WithLocation (FunctionInfo Resolved)
  -> [Type]
  -> Analyze m ()
validateFunction FunctionInfo {..} typeParameters = do
  let funcName = Name baseName typeParameters
  unlessM (uses vsValidated $ M.lookup constName) do
    vsValidated %= S.insert name
    funcType <- validateFunctionType _funType
    funcBody <- validateBlock _funBody
    let funcInfo = FunctionInfo funcType funcBody
    vsSymbols %= M.insert funcName funcInfo
    vsValueCache %= M.insert funcName (Typed funcType $ FunctionNameExpr funcName funcType)

validateType
  :: Monad m
  => PathInfo Resolved
  -> Analyze m Type
validateType PathInfo {..} = do
  params <- traverse validateType _pathParams
  case _pathName of
    BuiltinType IntName              -> pure IntType
    BuiltinType CharName             -> pure CharType
    BuiltinType BoolName             -> pure BoolType
    BuiltinType UnitName             -> pure UnitType
    BuiltinType VoidName             -> pure VoidType
    Struct baseName                  -> validateStruct baseName params
    Enum   baseName                  -> validateEnum   baseName params
    BuiltinFunction _                -> fatal $ ErrorNotAType _pathName
    Constant baseName                -> fatal $ ErrorNotAType _pathName
    Function baseName                -> fatal $ ErrorNotAType _pathName
    TypeAlias baseName               -> unimplemented
    TypeParameter name source        -> unimplemented
    Placeholder                      -> fatal $ ErrorNotAType _pathName
    FunctionPointer functionType     -> unimplemented
    FunctionArgument argName argType -> fatal $ ErrorNotAType _pathName
    LetVariable varName varType      -> fatal $ ErrorNotAType _pathName

typeMatches
  :: Type
  -> Type
  -> Bool
typeMatches a b =
  case (a,b) of
    (VoidType,        _)               -> True
    (_,               VoidType)        -> True
    (IntType,         IntType)         -> True
    (BoolType,        BoolType)        -> True
    (CharType,        CharType)        -> True
    (UnitType,        UnitType)        -> True
    (EnumType n1 _,   EnumType n2 _)   -> n1 == n2
    (StructType n1 _, StructType n2 _) -> n1 == n2
    (FunctionType f1, FunctionType f2) -> f1 == f2
    _                                  -> False
