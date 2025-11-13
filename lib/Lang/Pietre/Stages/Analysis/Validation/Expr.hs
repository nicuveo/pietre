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
import Lang.Pietre.Representations.AST.Common    as Common
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Monad


validateConstExpression
  :: Monad m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed ConstExpression)
validateConstExpression WithLocation {..} = do
  currentLocation .= _location
  case _located of
    Input.PathExpr path ->
      validateConstPathExpr path
    Input.CastExpr expr targetType -> do
      validatedExpr <- try $ validateConstExpression expr
      validatedType <- try $ validateConcreteType targetType
      ensure <$> liftA2 validateConstCastExpression validatedExpr validatedType
    Input.FieldAccessExpr expr field ->
      validateConstFieldAccessExpression expr field
    Input.CallExpr _ _ ->
      report unimplemented -- ErrorFunctionCallInConstExpression
    Input.ArrayExpr _ ->
      unimplemented
    Input.IndexExpr _ _ ->
      unimplemented
    Input.StructExpr structName fields -> do
      validateStructConstExpression structName fields
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
    Input.BoolNegationExpr expr ->
      validateBoolNegationConstExpression expr
    Input.IntNegationExpr expr ->
      validateIntNegationConstExpression expr
    Input.AdditionExpr e1 e2 ->
      validateBinaryExprWith validateAdditionConstExpression e1 e2
    Input.SubtractionExpr e1 e2 ->
      validateBinaryExprWith validateSubtractionConstExpression e1 e2
    Input.MultiplicationExpr e1 e2 ->
      validateBinaryExprWith validateMultiplicationConstExpression e1 e2
    Input.ExponentiationExpr e1 e2 ->
      validateBinaryExprWith validateExponentiationConstExpression e1 e2
    Input.DivisionExpr e1 e2 ->
      validateBinaryExprWith validateDivisionConstExpression e1 e2
    Input.ModuloExpr e1 e2 ->
      validateBinaryExprWith validateModuloConstExpression e1 e2
    Input.EqualityExpr e1 e2 ->
      validateBinaryExprWith validateEqualityConstExpression e1 e2
    Input.DifferenceExpr e1 e2 ->
      validateBinaryExprWith validateDifferenceConstExpression e1 e2
    Input.GreaterExpr e1 e2 ->
      validateBinaryExprWith validateGreaterConstExpression e1 e2
    Input.LesserExpr e1 e2 ->
      validateBinaryExprWith validateLesserConstExpression e1 e2
    Input.GreaterEqExpr e1 e2 ->
      validateBinaryExprWith validateGreaterEqConstExpression e1 e2
    Input.LesserEqExpr e1 e2 ->
      validateBinaryExprWith validateLesserEqConstExpression e1 e2
    Input.BoolAndExpr e1 e2 ->
      validateBinaryExprWith validateBoolAndConstExpression e1 e2
    Input.BoolOrExpr e1 e2 ->
      validateBinaryExprWith validateBoolOrConstExpression e1 e2
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
      lhs <- try $ validateConstExpression e1
      rhs <- try $ validateConstExpression e2
      ensure <$> liftA2 f lhs rhs

    validateSubtractionConstExpression    = validateBinaryIntConstExpression (pure ... subtract)
    validateMultiplicationConstExpression = validateBinaryIntConstExpression (pure ... (*))
    validateExponentiationConstExpression = validateBinaryIntConstExpression safeExp
    validateDivisionConstExpression       = validateBinaryIntConstExpression (fmap fst ... safeDivMod)
    validateModuloConstExpression         = validateBinaryIntConstExpression (fmap snd ... safeDivMod)
    validateBoolAndConstExpression        = validateBinaryBoolConstExpression (&&)
    validateBoolOrConstExpression         = validateBinaryBoolConstExpression (||)
    validateEqualityConstExpression       = validateBinaryCompareConstExpression (compareExpect (const False)) True
    validateDifferenceConstExpression     = validateBinaryCompareConstExpression (compareExpect (const True )) False
    validateGreaterConstExpression        = validateBinaryCompareConstExpression (compareExpect (== GT))       False
    validateLesserConstExpression         = validateBinaryCompareConstExpression (compareExpect (== LT))       False
    validateGreaterEqConstExpression      = validateBinaryCompareConstExpression (compareExpect (== GT))       True
    validateLesserEqConstExpression       = validateBinaryCompareConstExpression (compareExpect (== LT))       True

validateFunctionExpression
  :: Monad m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed Expression)
validateFunctionExpression WithLocation {..} = do
  currentLocation .= _location
  case _located of
    Input.PathExpr path ->
      validateFunctionPathExpr path
    Input.CastExpr expr targetType -> do
      validatedExpr <- try $ validateFunctionExpression expr
      validatedType <- try $ validateConcreteType targetType
      ensure <$> liftA2 validateFunctionCastExpression validatedExpr validatedType
    Input.FieldAccessExpr expr field ->
      validateFunctionFieldAccessExpression expr field
    Input.CallExpr functionPath functionArgs ->
      validateFunctionCallExpression functionPath functionArgs
    Input.ArrayExpr _ ->
      unimplemented
    Input.IndexExpr _ _ ->
      unimplemented
    Input.StructExpr structName fields -> do
      validateStructFunctionExpression structName fields
    Input.IntLiteralExpr i ->
      pure $ Typed IntType $ Output.IntLiteralExpr i
    Input.BoolLiteralExpr b ->
      pure $ Typed BoolType $ Output.BoolLiteralExpr b
    Input.CharLiteralExpr c ->
      pure $ Typed CharType $ Output.CharLiteralExpr c
    Input.StringLiteralExpr s ->
      unimplemented
    Input.ReferenceExpr expr ->
      report unimplemented -- ErrorReferenceExpressionOutsideOfFunctionCall
    Input.BoolNegationExpr expr -> do
      validateBoolNegationFunctionExpression expr
    Input.IntNegationExpr expr -> do
      validateIntNegationFunctionExpression expr
    Input.AdditionExpr e1 e2 ->
      validateBinaryExprWith validateAdditionFunctionExpression e1 e2
    Input.SubtractionExpr e1 e2 ->
      validateBinaryExprWith validateSubtractionFunctionExpression e1 e2
    Input.MultiplicationExpr e1 e2 ->
      validateBinaryExprWith validateMultiplicationFunctionExpression e1 e2
    Input.ExponentiationExpr e1 e2 ->
      validateBinaryExprWith validateExponentiationFunctionExpression e1 e2
    Input.DivisionExpr e1 e2 ->
      validateBinaryExprWith validateDivisionFunctionExpression e1 e2
    Input.ModuloExpr e1 e2 ->
      validateBinaryExprWith validateModuloFunctionExpression e1 e2
    Input.EqualityExpr e1 e2 ->
      validateBinaryExprWith validateEqualityFunctionExpression e1 e2
    Input.DifferenceExpr e1 e2 ->
      validateBinaryExprWith validateDifferenceFunctionExpression e1 e2
    Input.GreaterExpr e1 e2 ->
      validateBinaryExprWith validateGreaterFunctionExpression e1 e2
    Input.LesserExpr e1 e2 ->
      validateBinaryExprWith validateLesserFunctionExpression e1 e2
    Input.GreaterEqExpr e1 e2 ->
      validateBinaryExprWith validateGreaterEqFunctionExpression e1 e2
    Input.LesserEqExpr e1 e2 ->
      validateBinaryExprWith validateLesserEqFunctionExpression e1 e2
    Input.BoolAndExpr e1 e2 ->
      validateBinaryExprWith validateBoolAndFunctionExpression e1 e2
    Input.BoolOrExpr e1 e2 ->
      validateBinaryExprWith validateBoolOrFunctionExpression e1 e2
    RangeInclusiveExpr _ _ ->
      unimplemented
    RangeExclusiveExpr _ _ ->
      unimplemented
    AssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.AssignmentExpr (const pass) e1 e2
    AdditionAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.AdditionAssignmentExpr (expectType IntType) e1 e2
    SubtractionAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.SubtractionAssignmentExpr (expectType IntType) e1 e2
    MultiplicationAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.MultiplicationAssignmentExpr (expectType IntType) e1 e2
    DivisionAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.DivisionAssignmentExpr (expectType IntType) e1 e2
    ModuloAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.ModuloAssignmentExpr (expectType IntType) e1 e2
    ExponentiationAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.ExponentiationAssignmentExpr (expectType IntType) e1 e2
  where
    validateBinaryExprWith f e1 e2 = do
      lhs <- try $ validateFunctionExpression e1
      rhs <- try $ validateFunctionExpression e2
      ensure <$> liftA2 f lhs rhs

    validateSubtractionFunctionExpression    = validateBinaryIntFunctionExpression (pure ... subtract)
    validateMultiplicationFunctionExpression = validateBinaryIntFunctionExpression (pure ... (*))
    validateExponentiationFunctionExpression = validateBinaryIntFunctionExpression safeExp
    validateDivisionFunctionExpression       = validateBinaryIntFunctionExpression (fmap fst ... safeDivMod)
    validateModuloFunctionExpression         = validateBinaryIntFunctionExpression (fmap snd ... safeDivMod)
    validateBoolAndFunctionExpression        = validateBinaryBoolFunctionExpression (&&)
    validateBoolOrFunctionExpression         = validateBinaryBoolFunctionExpression (||)
    validateEqualityFunctionExpression       = validateBinaryCompareFunctionExpression (==)
    validateDifferenceFunctionExpression     = validateBinaryCompareFunctionExpression (/=)
    validateGreaterFunctionExpression        = validateBinaryCompareFunctionExpression (>)
    validateLesserFunctionExpression         = validateBinaryCompareFunctionExpression (<)
    validateGreaterEqFunctionExpression      = validateBinaryCompareFunctionExpression (>=)
    validateLesserEqFunctionExpression       = validateBinaryCompareFunctionExpression (<=)


validateRangeExpression
  :: Monad m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed RangeExpression)
validateRangeExpression _ = unimplemented

validateConstPathExpression
  :: Monad m
  => PathInfo Resolved
  -> ValidateT m (Typed ConstExpression)
validateConstPathExpression PathInfo {..} = do
  case _pathName of
    Constant name -> validateOtherConstValue name
    role          -> report $ ErrorNotAConst role
  where
    validateOtherConstValue name = do
      validateParamsCount [] _pathParams
      retrieveConstant name

validateFunctionPathExpression
  :: Monad m
  => PathInfo Resolved
  -> ValidateT m (Typed Expression)
validateFunctionPathExpression PathInfo {..} = do
  case _pathName of
    BuiltinFunction baseName         -> unimplemented
    Function baseName                -> validateFunctionPath baseName
    Constant baseName                -> validateConstPath baseName
    FunctionArgument argName argType -> validateArgPath argName argType
    LetVariable varName              -> validateVarPath varName
  where
    validateConstPath baseName = do
      validateParamsCount [] _pathParams
      fromConstExpression <$> retrieveConstant name

    validateVarPath varName = do
      varType <- retrieveVariableType varName
      pure $ Typed varType $ LocalVariableExpr varName

    validateArgPath argName = \case
      ByReference path -> do
        argType <- validateConcreteType path
        pure $ Typed argType $ ReferenceArgumentExpr argName
      ByValue path -> do
        argType <- validateConcreteType path
        pure $ Typed argType $ LocalVariableExpr argName

    validateFunctionPath baseName = do
      FunctionTypeInfo {..} <- retrieveFunctionType baseName
      validatedParams <- ensureNested $
        traverse (tryNested . validateConcreteType) _pathParams
      validateParamsCount _funParams validatedParams
      let
        paramMapping = M.fromList $ zip _funParams validatedParams
        validatedArguments = map (reifyType paramMapping) _funArgs
        validatedReturnType = reifyType paramMapping _funReturn
        validatedFunctionInfo = FunctionTypeInfo _funParams validatedArguments validatedReturnType
        name = Name baseName $ map assertName validatedParams
    unless (null _funParams) do
      functionDefinition <- retrieveFunctionDefinition baseName
      let request = FunctionInstantiationRequest
            { _firBaseName = baseName
            , _firDefinition = functionDefition
            , _firFunType = validatedFunctionInfo
            , _firParams = validatedParams
            }
      vsInstanceRequests %= (:|> request)
    pure $ Typed (FunctionType validatedFunctionInfo) $ FunctionNameExpr name validatedFunctionInfo

validateConstCastExpression
  :: Monad m
  => Typed ConstExpression
  -> Type
  -> ValidateT m (Typed ConstExpression)
validateConstCastExpression validatedExpr targetType =
  validateCastExpression validatedExpr targetType intResult charResult boolResult enumResult
  where
    intValue =
      case _typedValue validatedExpr of
        BoolLiteralConstExpr b -> fromEnum b
        IntLiteralConstExpr  i -> i
        CharLiteralConstExpr c -> ord c
        _ -> reportICE "const cast validation" "unexpected LHS" ["lhs: " ++ show validatedExpr]
    intResult =
      pure $ IntLiteralConstExpr intValue
    charResult =
      pure $ CharLiteralConstExpr $ chr intValue
    boolResult =
      pure $ BoolLiteralConstExpr $ intValue /= 0
    enumResult name values =
      if intValue < 0 || intValue >= length values
      then report $ ErrorEnumOutOfBounds name intValue
      else pure $ IntLiteralConstExpr intValue

validateFunctionCastExpression
  :: Monad m
  => Typed Expression
  -> Type
  -> ValidateT m (Typed Expression)
validateFunctionCastExpression validatedExpr targetType =
  validateCastExpression validatedExpr targetType intResult charResult boolResult enumResult
  where
    resultWith f =
      case _typedValue validatedExpr of
        BoolLiteralExpr b -> f $ fromEnum b
        IntLiteralExpr  i -> f $ i
        CharLiteralExpr c -> f $ ord c
        _                 -> pure $ CastExpr validatedExpr targetType
    intResult =
      resultWith (pure . IntLiteralExpr)
    charResult =
      resultWith (pure . CharLiteralExpr . chr)
    boolResult =
      resultWith (pure . BoolLiteralExpr . (/= 0))
    enumResult name values =
      resultWith \v ->
        if v < 0 || v >= length values
        then report $ ErrorEnumOutOfBounds name intValue
        else pure $ IntLiteralConstExpr intValue

validateCastExpression
  :: Monad m
  => Typed expr
  -> Type
  -> ValidateT m (Typed expr)
  -> ValidateT m (Typed expr)
  -> ValidateT m (Typed expr)
  -> (Name -> NonEmpty (Identifier, Name) -> ValidateT m (Typed expr))
  -> ValidateT m (Typed expr)
validateCastExpression validatedExpr targetType intResult charResult boolResult enumResult = do
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

validateConstFieldAccessExpression
  :: Monad m
  => Resolved.Expression
  -> Identifier
  -> ValidateT m (Typed ConstExpression)
validateConstFieldAccessExpression expr fieldName = do
  validatedExpr <- validateConstExpression expr
  case _typedValue validatedExpr of
    StructConstExpr _ fields -> do
      (_, fieldValue) <-
        find ((fieldName ==) . fst) fields `onNothing`
        report (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure fieldValue
    _ -> report $ ErrorFieldAccessNotAStruct (_typeInfo validatedExpr)

validateFunctionFieldAccessExpression
  :: Monad m
  => Resolved.Expression
  -> Identifier
  -> ValidateT m (Typed ConstExpression)
validateFunctionFieldAccessExpression expr fieldName = do
  validatedExpr <- validateFunctionExpression expr
  case _typeInfo validatedExpr of
    StructType StructTypeInfo {..} -> do
      StructInfo {..} <- retrieveStruct _structBaseName
      let
        paramMapping = M.fromList $ zip _structParams _structTypeParams
        validatedFields = map (reifyType paramMapping) _structValues
      (_, fieldType) <-
        find ((fieldName ==) . fst) validatedFields `onNothing`
        report (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure $ FieldAccessExpression (StructInfo _structParams validatedFields) fieldType fieldName
    wrongType ->
      report $ ErrorNotAStruct wrongType

validateFunctionCallExpression = unimplemented


validateStructConstExpression
  :: Monad m
  => PathInfo Resolved
  -> NonEmpty (Identifier, WithLocation (Expression Resolved))
  -> ValidateT m (Typed ConstExpression)
validateStructConstExpression structPath fields = do
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
  pure $ Typed (StructType validatedStructType) $ StructConstExpr structInfo validatedFields
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

validateBoolNegationConstExpression
  :: Monad m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed ConstExpression)
validateBoolNegationConstExpression expr = do
  value <- expectConstBool =<< validateConstExpression expr
  pure $ Typed BoolType (not value)

validateIntNegationConstExpression
  :: Monad m
  => WithLocation Resolved.Expression
  -> ValidateT m (Typed ConstExpression)
validateIntNegationConstExpression expr = do
  value <- expectConstBool =<< validateConstExpression expr
  pure $ Typed IntType (-value)

validateBinaryIntConstExpression
  :: Monad m
  => (Int -> Int -> ValidateT m Int)
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBinaryIntConstExpression f lhs rhs =
  Typed IntType . join <$> liftA2 f (expectConstInt lhs) (expectConstInt rhs)

validateBinaryBoolConstExpression
  :: Monad m
  => (Bool -> Bool -> Bool)
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBinaryBoolConstExpression f lhs rhs =
  Typed BoolType <$> liftA2 f (expectConstBool lhs) (expectConstBool rhs)

validateBinaryCompareConstExpression
  :: Monad m
  -> (forall a. Ord a => a -> a -> Maybe Bool)
  -> Bool
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateBinaryCompareConstExpression f defaultCase lhs rhs
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

validateAdditionConstExpression
  :: Monad m
  => Typed ConstExpression
  -> Typed ConstExpression
  -> ValidateT m (Typed ConstExpression)
validateAdditionConstExpression lhs rhs = do
  case _typeInfo lhs of
    IntType -> do
      intValue <- liftA2 (+) (expectConstInt lhs) (expectConstInt rhs)
      pure $ Typed IntType intValue
    _ ->
      report $ ErrorWrongType [IntType] (_typeInfo lhs)

validateAssignmentExpression
  :: Monad m
  => (Typed LValueExpression -> Typed Expression -> Typed Expression)
  -> (ConcreteType -> ValidateT m ())
  -> Resolved.Expression
  -> Resolved.Expression
  -> ValidateT m (Typed Validated.Expression)
validateAssignmentExpression cons typeValidationCallback lhs rhs = do
  attemptedLHS <- try $ validateLValueExpression lhs
  attemptedRHS <- try $ validateFunctionExpression rhs
  validatedLHS <- ensure attemptedLHS
  validatedRHS <- ensure attemptedRHS
  expectType (_typeInfo validatedLHS) (_typeInfo validatedRHS)
  typeValidationCallback (_typeInfo validatedRHS)
  pure $ cons validatedLHS validatedRHS


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

fromConstExpression
  :: ConstExpression
  -> Validated.Expression
fromConstExpression = \case
  BoolLiteralConstExpr   x -> BoolLiteralExpr   x
  IntLiteralConstExpr    x -> IntLiteralExpr    x
  CharLiteralConstExpr   x -> CharLiteralExpr   x
  StringLiteralConstExpr x -> StringLiteralExpr x
  ArrayConstExpr         x -> ArrayExpr $ map fromConstExpression x
  StructConstExpr        x -> StructExpr $ fmap3 fromConstExpression x
