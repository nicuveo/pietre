module Lang.Pietre.Stages.Analysis.Validation.Expr where

import "this" Prelude

import Control.Lens                              hiding (mapping, op)
import Control.Monad.Loops                       (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                 (hoistMaybe)
import Data.List qualified as L
import Data.HashMap.Strict.Extra                 qualified as M
import Data.HashSet                              qualified as S
import Data.Set                                  qualified as Set
import Data.Ordered.Set qualified as OSet

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.AST.Common    as Input
import Lang.Pietre.Representations.AST.Resolved  as Input
import Lang.Pietre.Representations.AST.Validated as Output
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Monad


validateConstExpr
  :: Monad m
  => WithLocation (Input.Expression Resolved)
  -> ValidateT m (Typed Output.ConstExpression)
validateConstExpr WithLocation {..} = do
  currentLocation .= _location
  case _located of
    Input.PathExpr path ->
      validateConstPathExpr path
    Input.CastExpr expr targetType -> do
      validatedExpr <- try $ validateConstExpr expr
      validatedType <- try $ validateConcreteType targetType
      ensure <$> liftA2 validateConstCastExpr validatedExpr validatedType
    Input.FieldAccessExpr expr field -> do
      validatedExpr <- validateConstExpr expr
      validateConstFieldAccessExpr validatedExpr field
    Input.CallExpr _ _ ->
      report unimplemented -- ErrorFunctionCallInConstExpression
    Input.ArrayExpr _ ->
      unimplemented
    Input.IndexExpr _ _ ->
      unimplemented
    Input.StructExpr structName fields -> do
      validateStructConstExpr structName fields
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
      validateBoolNegationConstExpr validatedExpr
    Input.IntNegationExpr expr -> do
      validatedExpr <- validateConstExpr expr
      validateIntNegationConstExpr validatedExpr
    Input.AdditionExpr e1 e2 ->
      validateBinaryExprWith validateAdditionConstExpr e1 e2
    Input.SubtractionExpr e1 e2 ->
      validateBinaryExprWith validateSubtractionConstExpr e1 e2
    Input.MultiplicationExpr e1 e2 ->
      validateBinaryExprWith validateMultiplicationConstExpr e1 e2
    Input.ExponentiationExpr e1 e2 ->
      validateBinaryExprWith validateExponentiationConstExpr e1 e2
    Input.DivisionExpr e1 e2 ->
      validateBinaryExprWith validateDivisionConstExpr e1 e2
    Input.ModuloExpr e1 e2 ->
      validateBinaryExprWith validateModuloConstExpr e1 e2
    Input.EqualityExpr e1 e2 ->
      validateBinaryExprWith validateEqualityConstExpr e1 e2
    Input.DifferenceExpr e1 e2 ->
      validateBinaryExprWith validateDifferenceConstExpr e1 e2
    Input.GreaterExpr e1 e2 ->
      validateBinaryExprWith validateGreaterConstExpr e1 e2
    Input.LesserExpr e1 e2 ->
      validateBinaryExprWith validateLesserConstExpr e1 e2
    Input.GreaterEqExpr e1 e2 ->
      validateBinaryExprWith validateGreaterEqConstExpr e1 e2
    Input.LesserEqExpr e1 e2 ->
      validateBinaryExprWith validateLesserEqConstExpr e1 e2
    Input.BoolAndExpr e1 e2 ->
      validateBinaryExprWith validateBoolAndConstExpr e1 e2
    Input.BoolOrExpr e1 e2 ->
      validateBinaryExprWith validateBoolOrConstExpr e1 e2
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
  where
    compareExpect
      :: Ord a
      => (Ordering -> Bool)
      -> a
      -> a
      -> Maybe Bool
    compareExpect checkOrdering x y =
      case compare x y of
        EQ -> Nothing
        o  -> Just $ checkOrdering o

    validateBinaryExprWith f e1 e2 = do
      lhs <- try $ validateConstExpr e1
      rhs <- try $ validateConstExpr e2
      ensure <$> liftA2 f lhs rhs

    validateSubtractionConstExpr    = validateBinaryIntConstExpr (pure ... subtract)
    validateMultiplicationConstExpr = validateBinaryIntConstExpr (pure ... (*))
    validateExponentiationConstExpr = validateBinaryIntConstExpr safeExp
    validateDivisionConstExpr       = validateBinaryIntConstExpr (fmap fst ... safeDivMod)
    validateModuloConstExpr         = validateBinaryIntConstExpr (fmap snd ... safeDivMod)
    validateBoolAndConstExpr        = validateBinaryBoolConstExpr (&&)
    validateBoolOrConstExpr         = validateBinaryBoolConstExpr (||)
    validateEqualityConstExpr       = validateBinaryCompareConstExpr (compareExpect (const False)) True
    validateDifferenceConstExpr     = validateBinaryCompareConstExpr (compareExpect (const True )) False
    validateGreaterConstExpr        = validateBinaryCompareConstExpr (compareExpect (== GT))       False
    validateLesserConstExpr         = validateBinaryCompareConstExpr (compareExpect (== LT))       False
    validateGreaterEqConstExpr      = validateBinaryCompareConstExpr (compareExpect (== GT))       True
    validateLesserEqConstExpr       = validateBinaryCompareConstExpr (compareExpect (== LT))       True


validateConstPathExpr
  :: Monad m
  => PathInfo Resolved
  -> ValidateT m (Typed Output.ConstExpression)
validateConstPathExpr PathInfo {..} = do
  case _pathName of
    Constant name -> validateOtherConstValue name
    role          -> report $ ErrorNotAConst role
  where
    validateOtherConstValue name = do
      let actual = length _pathParams
      when (actual /= 0) $
        report $ ErrorIncorrectTypeParameterCount name 0 actual
      retrieveConstant name

validateConstCastExpr
  :: Monad m
  => Typed Output.ConstExpression
  -> Output.Type
  -> ValidateT m (Typed Output.ConstExpression)
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
  -> ValidateT m (Typed Output.Expression)
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
  -> ValidateT m (Typed expr)
  -> ValidateT m (Typed expr)
  -> ValidateT m (Typed expr)
  -> (Name -> NonEmpty (Identifier, Name) -> ValidateT m (Typed expr))
  -> ValidateT m (Typed expr)
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
  -> ValidateT m (Typed ConstExpression)
validateConstFieldAccessExpr validatedExpr fieldName = do
  case _typedValue validatedExpr of
    StructConstExpr _ fields -> do
      (_fieldName, fieldValue) <-
        find ((fieldName ==) . fst) fields `onNothing`
        report (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure fieldValue
    _ -> report $ ErrorFieldAccessNotAStruct (_typeInfo validatedExpr)

validateFunctionFieldAccessExpr
  :: Monad m
  => Typed ConstExpression
  -> Identifier
  -> ValidateT m (Typed ConstExpression)
validateFunctionFieldAccessExpr validatedExpr fieldName = do
  case _typedValue validatedExpr of
    StructExpr _ fields -> do
      (_fieldName, fieldValue) <-
        find ((fieldName ==) . fst) fields `onNothing`
        fatal (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure fieldValue
    _ -> unimplemented

validateStructConstExpr
  :: Monad m
  => PathInfo Resolved
  -> NonEmpty (Identifier, WithLocation (Expression Resolved))
  -> ValidateT m (Typed ConstExpression)
validateStructConstExpr structPath fields = do
  attemptedType <- try $ validateStructType
  validatedFields <- ensure =<< getCompose (traverse2 (Compose . try . validatedConstExpr) fields)
  StructTypeInfo {..} <- ensure attemptedType
  structInfo@StructInfo {..} <- retrieveStruct _structBaseName
  let
    structFields = S.fromList $ map fst $ NE.toList _structValues
    paramTypes = M.fromList _structParams _structTypeParams

  -- check that all fields are "known"
  ensure =<<
    getCompose (
      for fields \(identifier, _) -> do
        unless (S.member identifier structFields) $
          Compose $ try $ report $ ErrorStructUnknownField _structBaseName identifier
    )

  -- check unicity of fields and collect parameter maps
  let fieldsMap = M.fromListWith (<>) $ NE.toList $ fmap2 pure values
  parameterMaps <- ensure =<<
    getCompose (traverse (Compose . try . validateField _structBaseName fieldsMap) _structValues)
  let parameterMap = unionsWith (<>) parameterMaps

  -- check unicity of type parameters and build parameter map
  structTypeParams <- ensure =<<
    getCompose (traverse (Compose . try . validateParam _structBaseName parametersMap) _structTypeParams)

  let validatedStructType = StructTypeInfo
        { _structBaseName   = _structBaseName
        , _structTypeParams = snd <$> structTypeParams
        }
  pure $ Typed (StructType validatedStructType) $ Output.StructConstExpr structInfo validatedFields
  where
    validateStructType =
      validatePartialType structPath >>= \case
        StructType info -> pure info
        otherType -> report $ NotAStruct otherType

    validateField baseName fieldsMap (fieldName, parameterizedType) = do
      case fold $ M.lookup fieldName fieldsMap of
        [] ->
          report $ ErrorStructMissingField baseName fieldName
        [expr] -> do
          mappings <- buildTypeParameterMap fieldType (_typeInfo expr)
          pure $ M.fromListWith (<>) $ fmap2 pure
        _ -> do
          report $ ErrorStructDuplicatedField baseName fieldName

    validateParam baseName typeMap (paramName, partialType) = do
      case fold $ M.lookup paramName typeMap of
        [] ->
          report $ ErrorStructAmbiguousType baseName paramName
        possibleTypes -> do
          unless (typesAllMatch possibleTypes) $
            report $ ErrorStructIncompatibleTypes baseName paramName possibleTypes
          let concreteType = findFirstNonVoid possibleTypes
          validateTypePattern partialType concreteType
          pure (paramName, concreteType)

    findFirstNonVoid = fromMaybe VoidType . find \case
      VoidType -> False
      _ -> True

validateBoolNegationConstExpr
  :: Monad m
  => Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBoolNegationConstExpr expr = do
  value <- expectConstBool expr
  pure $ Typed BoolType (not value)

validateIntNegationConstExpr
  :: Monad m
  => Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateIntNegationConstExpr expr = do
  value <- expectConstInt expr
  pure $ Typed IntType (-value)

validateBinaryIntConstExpr
  :: Monad m
  => (Int -> Int -> ValidateT m Int)
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBinaryIntConstExpr f lhs rhs =
  Typed IntType . join <$> liftA2 f (expectConstInt lhs) (expectConstInt rhs)

validateBinaryBoolConstExpr
  :: Monad m
  => (Bool -> Bool -> Bool)
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBinaryBoolConstExpr f lhs rhs =
  Typed BoolType <$> liftA2 f (expectConstBool lhs) (expectConstBool rhs)

validateBinaryCompareConstExpr
  :: Monad m
  -> (forall a. Ord a => a -> a -> Maybe Bool)
  -> Bool
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBinaryCompareConstExpr f defaultCase lhs rhs
  expectType (_typeInfo lhs) (_typeInfo rhs)
  pure $ Typed BoolType $ fromMaybe defaultCase $ go (_typedValue lhs) (_typedValue rhs)
  where
    go lValue rValue = case (lValue, rValue) of
      (StructConstExpr structType@StructType {..} lhsFields, StructConstExpr _ rhsFields) -> do
        StructInfo {..} <- retrieveStruct _structBaseName
        comparisons <- for _structValues \(fieldName, _) -> do
          (_, lhsField) <- findField structType lhsFields fieldName
          (_, rhsField) <- findField structType rhsFields fieldName
          go (_typedValue lhsField) (_typedValue rhsField)
        pure $ and comparisons
      (IntLiteralConstExpr  i1, IntLiteralConstExpr  i2) -> pure $ f i1 i2
      (CharLiteralConstExpr c1, CharLiteralConstExpr c2) -> pure $ f c1 c2
      (BoolLiteralConstExpr b1, BoolLiteralConstExpr b2) -> pure $ f b1 b2
      _ ->
        reportICE
        "const expr validation"
        "unknown or incompatible compile time values"
        [ "lhs: " ++ show e1
        , "rhs: " ++ show e2
        ]

    findField structType fields fieldName =
      find ((fieldName ==) . fst) fields `onNothing`
        reportICE
          "const comparison expression validation"
          "field not found in struct"
          [ "struct type: " ++ show structType
          , "struct fields: " ++ show fields
          ]

validateAdditionConstExpr
  :: Monad m
  => Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateAdditionConstExpr lhs rhs = do
  case _typeInfo lhs of
    IntType -> do
      intValue <- liftA2 (+) (expectConstInt lhs) (expectConstInt rhs)
      pure $ Typed IntType intValue
    _ ->
      report $ ErrorWrongType [IntType] (_typeInfo lhs)

safeDivMod
  :: Monad m
  => Int
  -> Int
  -> ValidateT m (Int, Int)
safeDiv x y = do
  when (y == 0) $ report ErrorDivideByZero
  pure $ x `divMod` y

safeExp
  :: Monad m
  => Int
  -> Int
  -> ValidateT m Int
safeExp x y = do
  when (y < 0) $ fatal ErrorNegativeExponent
  pure $ x ^ y
