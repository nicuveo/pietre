module Lang.Pietre.Stages.Analysis.Validation.Expr where

import "this" Prelude

import Control.Lens                                   hiding (cons, mapping, op,
                                                       (...))
import Data.HashMap.Strict.Extra                      qualified as M
import Data.HashSet                                   qualified as S
import Data.List.NonEmpty                             qualified as NE

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Resolved       as Resolved
import Lang.Pietre.Representations.AST.Validated      as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Context
import Lang.Pietre.Stages.Analysis.Validation.Expect
import Lang.Pietre.Stages.Analysis.Validation.Monad
import Lang.Pietre.Stages.Analysis.Validation.Types


validateConstExpression
  :: WithLocation Resolved.Expression
  -> Validate (Typed ConstExpression)
validateConstExpression WithLocation {..} = do
  currentLocation .= _location
  case _located of
    Resolved.PathExpr path ->
      validateConstPathExpression path
    Resolved.CastExpr expr targetType -> do
      attemptedExpr <- try $ validateConstExpression expr
      validatedType <- validateConcreteType targetType
      validatedExpr <- ensure attemptedExpr
      validateConstCastExpression validatedExpr validatedType
    Resolved.FieldAccessExpr expr field ->
      validateConstFieldAccessExpression expr field
    Resolved.CallExpr _ _ ->
      fatal unimplemented -- ErrorFunctionCallInConstExpression
    Resolved.ArrayExpr _ ->
      unimplemented
    Resolved.IndexExpr _ _ ->
      unimplemented
    Resolved.StructExpr baseName fields -> do
      validateStructConstExpression baseName fields
    Resolved.IntLiteralExpr i ->
      pure $ Typed IntType $ IntLiteralConstExpr i
    Resolved.BoolLiteralExpr b ->
      pure $ Typed BoolType $ BoolLiteralConstExpr b
    Resolved.CharLiteralExpr c ->
      pure $ Typed CharType $ CharLiteralConstExpr c
    Resolved.StringLiteralExpr _ ->
      unimplemented
    Resolved.ReferenceExpr _ ->
      fatal unimplemented -- ErrorReferenceExpressionInConstExpression
    Resolved.BoolNegationExpr expr ->
      validateBoolNegationConstExpression expr
    Resolved.IntNegationExpr expr ->
      validateIntNegationConstExpression expr
    Resolved.AdditionExpr e1 e2 ->
      validateBinaryExprWith validateAdditionConstExpression e1 e2
    Resolved.SubtractionExpr e1 e2 ->
      validateBinaryExprWith validateSubtractionConstExpression e1 e2
    Resolved.MultiplicationExpr e1 e2 ->
      validateBinaryExprWith validateMultiplicationConstExpression e1 e2
    Resolved.ExponentiationExpr e1 e2 ->
      validateBinaryExprWith validateExponentiationConstExpression e1 e2
    Resolved.DivisionExpr e1 e2 ->
      validateBinaryExprWith validateDivisionConstExpression e1 e2
    Resolved.ModuloExpr e1 e2 ->
      validateBinaryExprWith validateModuloConstExpression e1 e2
    Resolved.EqualityExpr e1 e2 ->
      validateBinaryExprWith validateEqualityConstExpression e1 e2
    Resolved.DifferenceExpr e1 e2 ->
      validateBinaryExprWith validateDifferenceConstExpression e1 e2
    Resolved.GreaterExpr e1 e2 ->
      validateBinaryExprWith validateGreaterConstExpression e1 e2
    Resolved.LesserExpr e1 e2 ->
      validateBinaryExprWith validateLesserConstExpression e1 e2
    Resolved.GreaterEqExpr e1 e2 ->
      validateBinaryExprWith validateGreaterEqConstExpression e1 e2
    Resolved.LesserEqExpr e1 e2 ->
      validateBinaryExprWith validateLesserEqConstExpression e1 e2
    Resolved.BoolAndExpr e1 e2 ->
      validateBinaryExprWith validateBoolAndConstExpression e1 e2
    Resolved.BoolOrExpr e1 e2 ->
      validateBinaryExprWith validateBoolOrConstExpression e1 e2
    Resolved.RangeInclusiveExpr _ _ ->
      unimplemented
    Resolved.RangeExclusiveExpr _ _ ->
      unimplemented
    Resolved.AssignmentExpr _ _ ->
      fatal unimplemented -- ErrorAssignmentInConstExpresssion
    Resolved.AdditionAssignmentExpr _ _ ->
      fatal unimplemented -- ErrorAssignmentInConstExpresssion
    Resolved.SubtractionAssignmentExpr _ _ ->
      fatal unimplemented -- ErrorAssignmentInConstExpresssion
    Resolved.MultiplicationAssignmentExpr _ _ ->
      fatal unimplemented -- ErrorAssignmentInConstExpresssion
    Resolved.DivisionAssignmentExpr _ _ ->
      fatal unimplemented -- ErrorAssignmentInConstExpresssion
    Resolved.ModuloAssignmentExpr _ _ ->
      fatal unimplemented -- ErrorAssignmentInConstExpresssion
    Resolved.ExponentiationAssignmentExpr _ _ ->
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
      join $ liftA2 f (ensure lhs) (ensure rhs)

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
  :: WithLocation Resolved.Expression
  -> Validate (Typed Validated.Expression)
validateFunctionExpression WithLocation {..} = do
  currentLocation .= _location
  case _located of
    Resolved.PathExpr path ->
      validateFunctionPathExpression path
    Resolved.CastExpr expr targetType -> do
      validatedExpr <- try $ validateFunctionExpression expr
      validatedType <- try $ validateConcreteType targetType
      join $ liftA2 validateFunctionCastExpression (ensure validatedExpr) (ensure validatedType)
    Resolved.FieldAccessExpr expr field ->
      validateFunctionFieldAccessExpression expr field
    Resolved.CallExpr functionPath functionArgs ->
      validateFunctionCallExpression functionPath functionArgs
    Resolved.ArrayExpr _ ->
      unimplemented
    Resolved.IndexExpr _ _ ->
      unimplemented
    Resolved.StructExpr baseName fields -> do
      validateStructFunctionExpression baseName fields
    Resolved.IntLiteralExpr i ->
      pure $ Typed IntType $ Validated.IntLiteralExpr i
    Resolved.BoolLiteralExpr b ->
      pure $ Typed BoolType $ Validated.BoolLiteralExpr b
    Resolved.CharLiteralExpr c ->
      pure $ Typed CharType $ Validated.CharLiteralExpr c
    Resolved.StringLiteralExpr _ ->
      unimplemented
    Resolved.ReferenceExpr _ ->
      fatal unimplemented -- ErrorReferenceExpressionOutsideOfFunctionCall
    Resolved.BoolNegationExpr expr -> do
      validateBoolNegationFunctionExpression expr
    Resolved.IntNegationExpr expr -> do
      validateIntNegationFunctionExpression expr
    Resolved.AdditionExpr e1 e2 ->
      validateBinaryExprWith validateAdditionFunctionExpression e1 e2
    Resolved.SubtractionExpr e1 e2 ->
      validateBinaryExprWith validateSubtractionFunctionExpression e1 e2
    Resolved.MultiplicationExpr e1 e2 ->
      validateBinaryExprWith validateMultiplicationFunctionExpression e1 e2
    Resolved.ExponentiationExpr e1 e2 ->
      validateBinaryExprWith validateExponentiationFunctionExpression e1 e2
    Resolved.DivisionExpr e1 e2 ->
      validateBinaryExprWith validateDivisionFunctionExpression e1 e2
    Resolved.ModuloExpr e1 e2 ->
      validateBinaryExprWith validateModuloFunctionExpression e1 e2
    Resolved.EqualityExpr e1 e2 ->
      validateBinaryExprWith validateEqualityFunctionExpression e1 e2
    Resolved.DifferenceExpr e1 e2 ->
      validateBinaryExprWith validateDifferenceFunctionExpression e1 e2
    Resolved.GreaterExpr e1 e2 ->
      validateBinaryExprWith validateGreaterFunctionExpression e1 e2
    Resolved.LesserExpr e1 e2 ->
      validateBinaryExprWith validateLesserFunctionExpression e1 e2
    Resolved.GreaterEqExpr e1 e2 ->
      validateBinaryExprWith validateGreaterEqFunctionExpression e1 e2
    Resolved.LesserEqExpr e1 e2 ->
      validateBinaryExprWith validateLesserEqFunctionExpression e1 e2
    Resolved.BoolAndExpr e1 e2 ->
      validateBinaryExprWith validateBoolAndFunctionExpression e1 e2
    Resolved.BoolOrExpr e1 e2 ->
      validateBinaryExprWith validateBoolOrFunctionExpression e1 e2
    Resolved.RangeInclusiveExpr _ _ ->
      unimplemented
    Resolved.RangeExclusiveExpr _ _ ->
      unimplemented
    Resolved.AssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.AssignmentExpr (const pass) e1 e2
    Resolved.AdditionAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.AdditionAssignmentExpr (expectType IntType) e1 e2
    Resolved.SubtractionAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.SubtractionAssignmentExpr (expectType IntType) e1 e2
    Resolved.MultiplicationAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.MultiplicationAssignmentExpr (expectType IntType) e1 e2
    Resolved.DivisionAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.DivisionAssignmentExpr (expectType IntType) e1 e2
    Resolved.ModuloAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.ModuloAssignmentExpr (expectType IntType) e1 e2
    Resolved.ExponentiationAssignmentExpr e1 e2 ->
      validateAssignmentExpression Validated.ExponentiationAssignmentExpr (expectType IntType) e1 e2
  where
    validateBinaryExprWith f e1 e2 = do
      lhs <- try $ validateFunctionExpression e1
      rhs <- try $ validateFunctionExpression e2
      join $ liftA2 f (ensure lhs) (ensure rhs)

    validateSubtractionFunctionExpression    = validateBinaryIntFunctionExpression (pure ... subtract) Validated.SubtractionExpr
    validateMultiplicationFunctionExpression = validateBinaryIntFunctionExpression (pure ... (*)) Validated.MultiplicationExpr
    validateExponentiationFunctionExpression = validateBinaryIntFunctionExpression safeExp Validated.ExponentiationExpr
    validateDivisionFunctionExpression       = validateBinaryIntFunctionExpression (fmap fst ... safeDivMod) Validated.DivisionExpr
    validateModuloFunctionExpression         = validateBinaryIntFunctionExpression (fmap snd ... safeDivMod) Validated.ModuloExpr
    validateBoolAndFunctionExpression        = validateBinaryBoolFunctionExpression (&&) Validated.BoolAndExpr
    validateBoolOrFunctionExpression         = validateBinaryBoolFunctionExpression (||) Validated.BoolOrExpr
    validateEqualityFunctionExpression       = validateBinaryCompareFunctionExpression (==) Validated.EqualityExpr
    validateDifferenceFunctionExpression     = validateBinaryCompareFunctionExpression (/=) Validated.DifferenceExpr
    validateGreaterFunctionExpression        = validateBinaryCompareFunctionExpression (>) Validated.GreaterExpr
    validateLesserFunctionExpression         = validateBinaryCompareFunctionExpression (<) Validated.LesserExpr
    validateGreaterEqFunctionExpression      = validateBinaryCompareFunctionExpression (>=) Validated.GreaterEqExpr
    validateLesserEqFunctionExpression       = validateBinaryCompareFunctionExpression (<=) Validated.LesserEqExpr

validateLValueExpression
  :: WithLocation Resolved.Expression
  -> Validate (Typed Validated.LValueExpression)
validateLValueExpression WithLocation {..} = do
  currentLocation .= _location
  case _located of
    Resolved.PathExpr path ->
      validateFunctionPathLValueExpression path
    Resolved.FieldAccessExpr expr field ->
      validateFunctionFieldAccessLValueExpression expr field
    Resolved.IndexExpr _ _ ->
      unimplemented
    incorrectExpression ->
      fatal $ ErrorRValueAssignment incorrectExpression

validateRangeExpression
  :: WithLocation Resolved.Expression
  -> Validate (ConcreteType, RangeExpression)
validateRangeExpression _ = unimplemented

validateConstPathExpression
  :: Resolved.PathInfo
  -> Validate (Typed ConstExpression)
validateConstPathExpression PathInfo {..} = do
  case _pathBase of
    Constant name -> validateOtherConstValue name
    role          -> fatal $ ErrorNotAConst role
  where
    validateOtherConstValue baseName = do
      validateParamsCount baseName [] _pathParams
      retrieveConstant baseName

validateFunctionPathExpression
  :: Resolved.PathInfo
  -> Validate (Typed Validated.Expression)
validateFunctionPathExpression PathInfo {..} = do
  case _pathBase of
    BuiltinFunction _baseName        -> unimplemented
    Function baseName                -> validateFunctionPath baseName
    Constant baseName                -> validateConstPath baseName
    FunctionArgument argName argType -> validateArgPath argName argType
    LetVariable varName              -> validateVarPath varName
    role                             -> fatal $ ErrorNotAValue role
  where
    validateConstPath baseName = do
      validateParamsCount baseName [] _pathParams
      fmap2 fromConstExpression $ retrieveConstant baseName

    validateVarPath varName = do
      varType <- retrieveVariableType varName
      pure $ Typed varType $ LocalVariableExpr varName

    validateArgPath argName = \case
      Resolved.ByReference path -> do
        argType <- validateConcreteType path
        pure $ Typed argType $ ReferenceArgumentExpr argName
      Resolved.ByValue path -> do
        argType <- validateConcreteType path
        pure $ Typed argType $ LocalVariableExpr argName

    reifyConcreteType
      :: HashMap Identifier ConcreteType
      -> ParameterizedType
      -> ConcreteType
    reifyConcreteType = reifyType @ConcreteFunctor

    validateFunctionPath baseName = do
      FunctionTypeInfo {..} <- retrieveFunctionType baseName
      validatedParams <- ensureNested $
        traverse (tryNested . validateConcreteType) _pathParams
      validateParamsCount baseName _funParams validatedParams
      let
        paramMapping = M.fromList $ zip _funParams validatedParams
        validatedArguments = fmap3 (reifyConcreteType paramMapping) _funArgs
        validatedReturnType = reifyConcreteType paramMapping _funReturn
        validatedFunctionInfo = FunctionTypeInfo _funParams validatedArguments validatedReturnType
        name = Name baseName $ map assertName validatedParams
      unless (null _funParams) do
        functionDefinition <- retrieveFunctionDefinition baseName
        let request = FunctionInstantiationRequest
              { _firBaseName = baseName
              , _firDefinition = functionDefinition
              , _firFunType = validatedFunctionInfo
              , _firParams = validatedParams
              }
        vsInstanceRequests %= (:|> request)
      pure $ Typed (Validated.FunctionType validatedFunctionInfo) $ FunctionNameExpr name validatedFunctionInfo

validateFunctionPathLValueExpression
  :: Resolved.PathInfo
  -> Validate (Typed Validated.LValueExpression)
validateFunctionPathLValueExpression PathInfo {..} = do
  case _pathBase of
    FunctionArgument argName argType -> validateArgPath argName argType
    LetVariable varName              -> validateVarPath varName
    role                             -> fatal $ ErrorNotAnLValue role
  where
    validateVarPath varName = do
      varType <- retrieveVariableType varName
      pure $ Typed varType $ LocalVariableLExpr varName

    validateArgPath argName = \case
      Resolved.ByReference path -> do
        argType <- validateConcreteType path
        pure $ Typed argType $ ReferenceArgumentLExpr argName
      Resolved.ByValue path -> do
        argType <- validateConcreteType path
        pure $ Typed argType $ LocalVariableLExpr argName

validateConstCastExpression
  :: HasCallStack
  => Typed ConstExpression
  -> ConcreteType
  -> Validate (Typed ConstExpression)
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
      then fatal $ ErrorEnumOutOfBounds name intValue
      else pure $ IntLiteralConstExpr intValue

validateFunctionCastExpression
  :: Typed Validated.Expression
  -> ConcreteType
  -> Validate (Typed Validated.Expression)
validateFunctionCastExpression validatedExpr targetType =
  validateCastExpression validatedExpr targetType intResult charResult boolResult enumResult
  where
    resultWith f =
      case _typedValue validatedExpr of
        Validated.BoolLiteralExpr b -> f $ fromEnum b
        Validated.IntLiteralExpr  i -> f $ i
        Validated.CharLiteralExpr c -> f $ ord c
        _                          -> pure $ Validated.CastExpr validatedExpr targetType
    intResult =
      resultWith (pure . Validated.IntLiteralExpr)
    charResult =
      resultWith (pure . Validated.CharLiteralExpr . chr)
    boolResult =
      resultWith (pure . Validated.BoolLiteralExpr . (/= 0))
    enumResult name values =
      resultWith \v ->
        if v < 0 || v >= length values
        then fatal $ ErrorEnumOutOfBounds name v
        else pure $ Validated.IntLiteralExpr v

validateCastExpression
  :: Typed expr
  -> ConcreteType
  -> Validate expr
  -> Validate expr
  -> Validate expr
  -> (BaseName -> [Identifier] -> Validate expr)
  -> Validate (Typed expr)
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
      (EnumType _ _, IntType) ->
        intResult
      (IntType, EnumType baseName values) ->
        enumResult baseName values
      (EnumType _ _, EnumType baseName values) ->
        enumResult baseName values
      _ ->
        fatal $ ErrorWrongCast (_typeInfo validatedExpr) targetType
  pure $ Typed targetType innerValue

validateConstFieldAccessExpression
  :: WithLocation Resolved.Expression
  -> Identifier
  -> Validate (Typed ConstExpression)
validateConstFieldAccessExpression expr fieldName = do
  validatedExpr <- validateConstExpression expr
  case _typedValue validatedExpr of
    StructConstExpr _ fields -> do
      (_, fieldValue) <-
        find ((fieldName ==) . fst) fields `onNothing`
        fatal (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      pure fieldValue
    _ -> fatal $ ErrorFieldAccessNotAStruct (_typeInfo validatedExpr)

validateFunctionFieldAccessExpression
  :: WithLocation Resolved.Expression
  -> Identifier
  -> Validate (Typed Validated.Expression)
validateFunctionFieldAccessExpression expr fieldName = do
  validatedExpr <- validateFunctionExpression expr
  validateFunctionFieldAccess Validated.FieldAccessExpr validatedExpr fieldName

validateFunctionFieldAccessLValueExpression
  :: WithLocation Resolved.Expression
  -> Identifier
  -> Validate (Typed Validated.LValueExpression)
validateFunctionFieldAccessLValueExpression expr fieldName = do
  validatedExpr <- validateLValueExpression expr
  validateFunctionFieldAccess Validated.FieldAccessLExpr validatedExpr fieldName

validateFunctionFieldAccess
  :: (Validated.StructInfo ConcreteFunctor -> Typed e -> Identifier -> e)
  -> Typed e
  -> Identifier
  -> Validate (Typed e)
validateFunctionFieldAccess cons validatedExpr fieldName = do
  case _typeInfo validatedExpr of
    StructType StructTypeInfo {..} -> do
      Validated.StructInfo {..} <- retrieveStruct _structBaseName
      let
        paramMapping = M.fromList $ zip _structParams _structTypeParams
        validatedFields = fmap2 (reifyType @ConcreteFunctor paramMapping) _structValues
      (_, fieldType) <-
        find ((fieldName ==) . fst) validatedFields `onNothing`
        fatal (ErrorFieldAccessFieldNotFound (_typeInfo validatedExpr) fieldName)
      let validatedStructInfo = Validated.StructInfo _structParams validatedFields
      pure $ Typed fieldType $ cons validatedStructInfo validatedExpr fieldName
    wrongType ->
      fatal $ ErrorNotAStruct $ concreteToPartial wrongType

validateFunctionCallExpression
  :: Resolved.PathInfo
  -> [WithLocation Resolved.Expression]
  -> Validate (Typed Validated.Expression)
validateFunctionCallExpression PathInfo {..} functionArgs =
  case _pathBase of
    BuiltinFunction _baseName ->
      unimplemented
    LetVariable identifier ->
      validateFunctionCallLetExpression identifier
    Function functionName ->
      validateFunctionCallFunctionName functionName
    role ->
      fatal $ ErrorNotAFunctionRole role

  where
    validateFunctionCallLetExpression identifier = do
      functionTypeInfo@FunctionTypeInfo {..} <- retrieveVariableType identifier >>= \case
        Validated.FunctionType info -> pure info
        otherType -> fatal $ ErrorNotAFunctionType otherType

      -- TODO: document / fix this
      declarationName <- use currentName
      let fakeName = BaseName
            (_nameModule declarationName)
            (_nameIdent declarationName <> "." <> identifier)

      -- validate number of params
      validateParamsCount fakeName [] _pathParams

      -- validate arguments
      validatedArgs <-
        ensureNested $ for (zip _funArgs functionArgs) \((argName, argType), argExpression) ->
          tryNested do
            (actualType, validatedArg) <- validateFunctionCallArgument @ConcreteFunctor (argName, argType) argExpression
            expectType actualType $ _typeInfo validatedArg
            pure validatedArg

      pure $ Typed _funReturn $ VariableCallExpr identifier functionTypeInfo validatedArgs

    validateFunctionCallFunctionName functionBaseName = do
      FunctionTypeInfo {..} <- retrieveFunctionType functionBaseName

      -- validate number of params
      validateParamsCountWith (<=) functionBaseName _funParams _pathParams
      partialParams <- ensureNested $ traverse (tryNested . validatePartialType) _pathParams
      let namedPartialParams = zip _funParams $ partialParams ++ repeat Nothing

      -- validate arguments
      validatedFunctionArgs <- ensureNested $ fmap2 snd $ zipWithM
        (tryNested ... validateFunctionCallArgument @ParameterizedFunctor)
        _funArgs
        functionArgs

      -- collect parameter maps
      parameterMaps <- ensureNested $ zipWithM
        (tryNested ... buildTypeParameterMap)
        (functionArgType . snd <$> _funArgs)
        (_typeInfo <$> validatedFunctionArgs)
      let parametersMap = unionsWith (<>) $ fmap2 pure $ map M.fromList parameterMaps

      -- check unicity of type parameters and build parameter map
      validatedTypeList <- ensureNested $
        traverse (tryNested . validateParam parametersMap functionBaseName) namedPartialParams
      let
        finalMapping = M.fromList validatedTypeList
        functionTypeParams = snd <$> validatedTypeList
        validatedReturnType = reifyType @ConcreteFunctor finalMapping _funReturn

      -- register function for instantiation
      let concreteFunctionTypeInfo = FunctionTypeInfo
            { _funParams = _funParams
            , _funArgs = fmap3 (reifyType @ConcreteFunctor finalMapping) _funArgs
            , _funReturn = validatedReturnType
            }
      unless (null _funParams) do
        functionDefinition <- retrieveFunctionDefinition functionBaseName
        let request = FunctionInstantiationRequest
              { _firBaseName = functionBaseName
              , _firDefinition = functionDefinition
              , _firFunType = concreteFunctionTypeInfo
              , _firParams = functionTypeParams
              }
        vsInstanceRequests %= (:|> request)

      let functionName = Name functionBaseName $ map assertName functionTypeParams
      pure $ Typed validatedReturnType $ FunctionCallExpr functionName concreteFunctionTypeInfo validatedFunctionArgs

    validateFunctionCallArgument
      :: (Identifier, FunctionArgType (TypeTree f))
      -> WithLocation Resolved.Expression
      -> Validate (TypeTree f, Typed Validated.Expression)
    validateFunctionCallArgument (argName, argType) argExpression =
      case argType of
        Validated.ByValue actualType ->
          (actualType,) <$> validateFunctionExpression argExpression
        Validated.ByReference actualType ->
          case _located argExpression of
            ReferenceExpr subExpr -> do
              validatedSubExpr <- validateFunctionPathExpression subExpr
              case _typedValue validatedSubExpr of
                LocalVariableExpr     _ -> pure (actualType, validatedSubExpr)
                ReferenceArgumentExpr _ -> pure (actualType, validatedSubExpr)
                invalidExpr -> fatal $ ErrorReferenceNotLocalVariable invalidExpr
            _ ->
              fatal $ ErrorFunctionCallArgExpectingReference argName

    validateParam
      :: HashMap Identifier [ConcreteType]
      -> BaseName
      -> (Identifier, PartialType)
      -> Validate (Identifier, ConcreteType)
    validateParam typeMap baseName (paramName, partialType) = do
      (paramName, ) <$>
        case fold $ M.lookup paramName typeMap of
          [] ->
            concretizeType partialType `onNothing`
              fatal (ErrorFunctionAmbiguousType baseName paramName)
          possibleTypes -> do
            unless (typesAllMatch possibleTypes) $
              fatal $ ErrorFunctionIncompatibleTypes baseName paramName possibleTypes
            let concreteType = findFirstNonVoid possibleTypes
            validateTypePattern partialType concreteType
            pure concreteType

    findFirstNonVoid = fromMaybe VoidType . find \case
      VoidType -> False
      _ -> True

validateStructConstExpression
  :: Resolved.PathInfo
  -> NonEmpty (Identifier, WithLocation Resolved.Expression)
  -> Validate (Typed ConstExpression)
validateStructConstExpression =
  validateStructExpression validateConstExpression StructConstExpr

validateStructFunctionExpression
  :: Resolved.PathInfo
  -> NonEmpty (Identifier, WithLocation Resolved.Expression)
  -> Validate (Typed Validated.Expression)
validateStructFunctionExpression =
  validateStructExpression validateFunctionExpression Validated.StructExpr

validateStructExpression
  :: (WithLocation Resolved.Expression -> Validate (Typed e))
  -> (    Validated.StructInfo ConcreteFunctor
       -> (NonEmpty (Identifier, Typed e))
       -> e
     )
  -> Resolved.PathInfo
  -> NonEmpty (Identifier, WithLocation Resolved.Expression)
  -> Validate (Typed e)
validateStructExpression fieldValidationCallback resultConstructor structPath fields = do
  attemptedType <- try validateStructType
  validatedFields <- ensureNested $ traverse2 (tryNested . fieldValidationCallback) fields
  StructTypeInfo {..} <- ensure attemptedType
  Validated.StructInfo {..} <- retrieveStruct _structBaseName
  let
    structFields = S.fromList $ map fst $ NE.toList _structValues
    paramTypes = zip _structParams (_structTypeParams ++ repeat Nothing)

  -- check that all fields are "known"
  ensureNested $
    for fields \(identifier, _) ->
      tryNested $
        unless (S.member identifier structFields) $
          fatal $ ErrorStructUnknownField _structBaseName identifier

  -- check unicity of fields and collect parameter maps
  let fieldsMap = M.fromListWith (<>) $ NE.toList $ fmap2 pure validatedFields
  parameterMaps <- ensureNested $
    traverse (tryNested . validateField _structBaseName fieldsMap) _structValues
  let parametersMap = unionsWith (<>) parameterMaps

  -- check unicity of type parameters and build parameter map
  validatedTypeParams <- ensureNested $
    traverse (tryNested . validateParam _structBaseName parametersMap) paramTypes

  let
    finalMapping = M.fromList $ zip _structParams validatedTypeParams
    validatedStructType = StructTypeInfo
      { _structBaseName   = _structBaseName
      , _structTypeParams = validatedTypeParams
      }
    validatedStructInfo = Validated.StructInfo
      { _structParams
      , _structValues = fmap2 (reifyType @ConcreteFunctor finalMapping) _structValues
      }
  pure $ Typed (StructType validatedStructType) $ resultConstructor validatedStructInfo validatedFields
  where
    validateStructType =
      validateNonEmptyPartialType structPath >>= \case
        StructType info -> pure info
        otherType -> fatal $ ErrorNotAStruct (Just otherType)

    validateField baseName fieldsMap (fieldName, parameterizedType) = do
      case fold $ M.lookup fieldName fieldsMap of
        [] ->
          fatal $ ErrorStructMissingField baseName fieldName
        [expr] -> do
          mappings <- buildTypeParameterMap parameterizedType (_typeInfo expr)
          pure $ M.fromListWith (<>) $ fmap2 pure mappings
        _ -> do
          fatal $ ErrorStructDuplicatedField baseName fieldName

    validateParam
      :: BaseName
      -> HashMap Identifier [ConcreteType]
      -> (Identifier, PartialType)
      -> Validate ConcreteType
    validateParam baseName typeMap (paramName, partialType) = do
      case fold $ M.lookup paramName typeMap of
        [] -> do
          concretizeType partialType `onNothing`
            fatal (ErrorStructAmbiguousType baseName paramName)
        possibleTypes -> do
          unless (typesAllMatch possibleTypes) $
            fatal $ ErrorStructIncompatibleTypes baseName paramName possibleTypes
          let concreteType = findFirstNonVoid possibleTypes
          validateTypePattern partialType concreteType
          pure concreteType

    findFirstNonVoid = fromMaybe VoidType . find \case
      VoidType -> False
      _ -> True

validateBoolNegationConstExpression
  :: WithLocation Resolved.Expression
  -> Validate (Typed ConstExpression)
validateBoolNegationConstExpression expr = do
  value <- expectConstBool =<< validateConstExpression expr
  pure $ Typed BoolType $ BoolLiteralConstExpr (not value)

validateBoolNegationFunctionExpression
  :: WithLocation Resolved.Expression
  -> Validate (Typed Validated.Expression)
validateBoolNegationFunctionExpression expr = do
  validatedExpr <- validateFunctionExpression expr
  expectType BoolType $ _typeInfo validatedExpr
  pure $ Typed BoolType $ case _typedValue validatedExpr of
    Validated.BoolLiteralExpr b -> Validated.BoolLiteralExpr (not b)
    _                           -> Validated.BoolNegationExpr validatedExpr

validateIntNegationConstExpression
  :: WithLocation Resolved.Expression
  -> Validate (Typed ConstExpression)
validateIntNegationConstExpression expr = do
  value <- expectConstInt =<< validateConstExpression expr
  pure $ Typed IntType $ IntLiteralConstExpr (-value)

validateIntNegationFunctionExpression
  :: WithLocation Resolved.Expression
  -> Validate (Typed Validated.Expression)
validateIntNegationFunctionExpression expr = do
  validatedExpr <- validateFunctionExpression expr
  expectType IntType $ _typeInfo validatedExpr
  pure $ Typed IntType $ case _typedValue validatedExpr of
    Validated.IntLiteralExpr i -> Validated.IntLiteralExpr (-i)
    _                          -> Validated.IntNegationExpr validatedExpr

validateBinaryIntConstExpression
  :: (Int -> Int -> Validate Int)
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> Validate (Typed ConstExpression)
validateBinaryIntConstExpression f lhs rhs = do
  lhsValue <- expectConstInt lhs
  rhsValue <- expectConstInt rhs
  Typed IntType . IntLiteralConstExpr <$> f lhsValue rhsValue

validateBinaryIntFunctionExpression
  :: (Int -> Int -> Validate Int)
  -> (Typed Validated.Expression -> Typed Validated.Expression -> Validated.Expression)
  -> Typed Validated.Expression
  -> Typed Validated.Expression
  -> Validate (Typed Validated.Expression)
validateBinaryIntFunctionExpression f c lhs rhs = do
  expectType IntType $ _typeInfo lhs
  expectType IntType $ _typeInfo rhs
  Typed IntType <$> case (_typedValue lhs, _typedValue rhs) of
    (Validated.IntLiteralExpr i1, Validated.IntLiteralExpr i2) ->
      Validated.IntLiteralExpr <$> f i1 i2
    _ ->
      pure $ c lhs rhs

validateBinaryBoolConstExpression
  :: (Bool -> Bool -> Bool)
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> Validate (Typed ConstExpression)
validateBinaryBoolConstExpression f lhs rhs = do
  lhsValue <- expectConstBool lhs
  rhsValue <- expectConstBool rhs
  pure $ Typed BoolType $ BoolLiteralConstExpr $ f lhsValue rhsValue

validateBinaryBoolFunctionExpression
  :: (Bool -> Bool -> Bool)
  -> (Typed Validated.Expression -> Typed Validated.Expression -> Validated.Expression)
  -> Typed Validated.Expression
  -> Typed Validated.Expression
  -> Validate (Typed Validated.Expression)
validateBinaryBoolFunctionExpression f c lhs rhs = do
  expectType BoolType $ _typeInfo lhs
  expectType BoolType $ _typeInfo rhs
  pure $ Typed BoolType $ case (_typedValue lhs, _typedValue rhs) of
    (Validated.BoolLiteralExpr b1, Validated.BoolLiteralExpr b2) ->
      Validated.BoolLiteralExpr $ f b1 b2
    _ ->
      c lhs rhs

validateBinaryCompareConstExpression
  :: (forall a. Ord a => a -> a -> Maybe Bool)
  -> Bool
  -> Typed ConstExpression
  -> Typed ConstExpression
  -> Validate (Typed ConstExpression)
validateBinaryCompareConstExpression f defaultValue lhs rhs = do
  expectType (_typeInfo lhs) (_typeInfo rhs)
  result <- go (_typedValue lhs) (_typedValue rhs) `onNothingM`
    pure defaultValue
  pure $ Typed BoolType $ BoolLiteralConstExpr result
  where
    go lValue rValue = case (lValue, rValue) of
      (StructConstExpr structType@Validated.StructInfo {..} lhsFields, StructConstExpr _ rhsFields) -> do
        comparisons <- for _structValues \(fieldName, _) -> do
          (_, lhsField) <- findField structType lhsFields fieldName
          (_, rhsField) <- findField structType rhsFields fieldName
          go (_typedValue lhsField) (_typedValue rhsField)
        pure $ asum comparisons
      (IntLiteralConstExpr  i1, IntLiteralConstExpr  i2) ->
        pure $ f i1 i2
      (CharLiteralConstExpr c1, CharLiteralConstExpr c2) ->
        pure $ f c1 c2
      (BoolLiteralConstExpr b1, BoolLiteralConstExpr b2) ->
        pure $ f b1 b2
      _ ->
        reportICE
        "const expr validation"
        "unknown or incompatible compile time values"
        [ "lhs: " ++ show lValue
        , "rhs: " ++ show rValue
        ]

    findField structType fields fieldName =
      find ((fieldName ==) . fst) fields `onNothing`
        reportICE
          "const comparison expression validation"
          "field not found in struct"
          [ "struct type: " ++ show structType
          , "struct fields: " ++ show fields
          ]

validateBinaryCompareFunctionExpression
  :: (forall a. Ord a => a -> a -> Bool)
  -> (Typed Validated.Expression -> Typed Validated.Expression -> Validated.Expression)
  -> Typed Validated.Expression
  -> Typed Validated.Expression
  -> Validate (Typed Validated.Expression)
validateBinaryCompareFunctionExpression f c lhs rhs = do
  expectType (_typeInfo lhs) (_typeInfo rhs)
  pure $ Typed BoolType $ case (_typedValue lhs, _typedValue rhs) of
    (Validated.IntLiteralExpr i1, Validated.IntLiteralExpr i2) ->
      Validated.BoolLiteralExpr $ f i1 i2
    (Validated.CharLiteralExpr c1, Validated.CharLiteralExpr c2) ->
      Validated.BoolLiteralExpr $ f c1 c2
    (Validated.BoolLiteralExpr b1, Validated.BoolLiteralExpr b2) ->
      Validated.BoolLiteralExpr $ f b1 b2
    _ ->
      c lhs rhs

validateAdditionConstExpression
  :: Typed ConstExpression
  -> Typed ConstExpression
  -> Validate (Typed ConstExpression)
validateAdditionConstExpression lhs rhs = do
  case _typeInfo lhs of
    IntType -> do
      intValue <- liftA2 (+) (expectConstInt lhs) (expectConstInt rhs)
      pure $ Typed IntType $ IntLiteralConstExpr intValue
    _ ->
      fatal $ ErrorWrongType [IntType] (_typeInfo lhs)

validateAdditionFunctionExpression
  :: Typed Validated.Expression
  -> Typed Validated.Expression
  -> Validate (Typed Validated.Expression)
validateAdditionFunctionExpression lhs rhs = do
  case _typeInfo lhs of
    IntType -> do
      expectType IntType $ _typeInfo rhs
      pure $ Typed IntType $ case (_typedValue lhs, _typedValue rhs) of
        (Validated.IntLiteralExpr i1, Validated.IntLiteralExpr i2) ->
          Validated.IntLiteralExpr (i1 + i2)
        _ ->
          Validated.AdditionExpr lhs rhs
    _ ->
      fatal $ ErrorWrongType [IntType] (_typeInfo lhs)

validateAssignmentExpression
  :: (Typed LValueExpression -> Typed Validated.Expression -> Validated.Expression)
  -> (ConcreteType -> Validate ())
  -> WithLocation Resolved.Expression
  -> WithLocation Resolved.Expression
  -> Validate (Typed Validated.Expression)
validateAssignmentExpression cons typeValidationCallback lhs rhs = do
  attemptedLHS <- try $ validateLValueExpression lhs
  attemptedRHS <- try $ validateFunctionExpression rhs
  validatedLHS <- ensure attemptedLHS
  validatedRHS <- ensure attemptedRHS
  expectType (_typeInfo validatedLHS) (_typeInfo validatedRHS)
  typeValidationCallback (_typeInfo validatedRHS)
  pure $ Typed UnitType $ cons validatedLHS validatedRHS


safeDivMod
  :: Int
  -> Int
  -> Validate (Int, Int)
safeDivMod x y = do
  when (y == 0) $ fatal ErrorDivideByZero
  pure $ x `divMod` y

safeExp
  :: Int
  -> Int
  -> Validate Int
safeExp x y = do
  when (y < 0) $ fatal ErrorNegativeExponent
  pure $ x ^ y

fromConstExpression
  :: ConstExpression
  -> Validated.Expression
fromConstExpression = \case
  BoolLiteralConstExpr   x -> Validated.BoolLiteralExpr   x
  IntLiteralConstExpr    x -> Validated.IntLiteralExpr    x
  CharLiteralConstExpr   x -> Validated.CharLiteralExpr   x
  StringLiteralConstExpr x -> Validated.StringLiteralExpr x
  ArrayConstExpr         x -> Validated.ArrayExpr $ fmap2 fromConstExpression x
  StructConstExpr   info x -> Validated.StructExpr info $ fmap3 fromConstExpression x
