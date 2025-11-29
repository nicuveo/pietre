module Lang.Pietre.Stages.Analysis.Validation
  ( validate
  , validateDefinition
  ) where

import "this" Prelude

import Control.Lens                                         hiding (mapping, op)
import Data.Functor.Compose
import Data.HashMap.Strict.Extra                            qualified as M
import Data.HashSet                                         qualified as S
import Data.List                                            qualified as L
import Data.Set.Ordered                                     qualified as OSet

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Resolved             as Resolved
import Lang.Pietre.Representations.AST.Validated            as Validated
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Context
import Lang.Pietre.Stages.Analysis.Validation.Expect
import Lang.Pietre.Stages.Analysis.Validation.Expr
import Lang.Pietre.Stages.Analysis.Validation.Instantiation
import Lang.Pietre.Stages.Analysis.Validation.Monad
import Lang.Pietre.Stages.Analysis.Validation.Types


--------------------------------------------------------------------------------
-- API

validate
  :: MonadDiagnosis m
  => DefinitionCache
  -> FunctionCache
  -> SymbolCache
  -> HashMap BaseName (WithLocation Resolved.Definition)
  -> m ( DefinitionCache
       , FunctionCache
       , SymbolCache
       )
validate definitionCache functionCache symbolCache localDefinitions = do
  let validateInfo = ValidateInfo
        { _viDefinitions      = definitionCache
        , _viFunctions        = functionCache
        , _viSymbols          = symbolCache
        , _viLocalDefinitions = localDefinitions
        }
  (ValidateState {..}, symbols) <- runValidate validateInfo do
    void $ ensureNested $
      M.forWithKey localDefinitions \declarationName resolvedDeclaration ->
        tryNested $
          validateDefinition declarationName resolvedDeclaration
    instantiateAllSymbols
  pure (_vsDefinitions, _vsFunctions, symbols)

validateDefinition
  :: BaseName
  -> WithLocation Resolved.Definition
  -> Validate (Maybe Validated.Definition)
validateDefinition baseName WithLocation {..} = do
  alreadyValidated <- uses vsValidated $ S.member baseName
  if alreadyValidated
  then lookupLocalDefinition baseName
  else do
    defStack <- use vsDefinitionStack
    if baseName `OSet.member` defStack
    then
      fatal $ ErrorCyclicDefinition baseName $ snd $ L.break (== baseName) $ toList defStack
    else do
      vsDefinitionStack %= (OSet.|> baseName)
      result <- try performValidation
      vsDefinitionStack %= OSet.delete baseName
      vsValidated %= S.insert baseName
      pure result
  where
    performValidation = do
      result <-
        withContext baseName _location $
          case _located of
            Resolved.TypeAliasDef info ->
              Validated.TypeAliasDef <$> validateTypeAlias info
            Resolved.StructDef info ->
              Validated.StructDef <$> validateStruct info
            Resolved.ConstDef info ->
              Validated.ConstDef <$> validateConst info
            Resolved.FunctionDef info ->
              Validated.FunctionDef <$> validateFunctionType info
            Resolved.EnumDef info ->
              pure $ Validated.EnumDef info
      vsDefinitions %= M.insert baseName result
      pure result


--------------------------------------------------------------------------------
-- Implementation

validateTypeAlias
  :: Resolved.TypeAliasInfo
  -> Validate Validated.TypeAliasInfo
validateTypeAlias Resolved.TypeAliasInfo {..} = do
  validatedValue <- validateParameterizedType _aliasValue
  pure $ Validated.TypeAliasInfo _aliasParams validatedValue

validateConst
  :: Resolved.ConstInfo
  -> Validate (Typed Validated.ConstExpression)
validateConst Resolved.ConstInfo {..} = do
  attemptedType <- try $ validateConcreteType _constType
  attemptedExpr <- try $ validateConstExpression _constExpr
  validatedType <- ensure attemptedType
  validatedExpr <- ensure attemptedExpr
  expectType validatedType (_typeInfo validatedExpr)
  pure validatedExpr

validateStruct
  :: Resolved.StructInfo
  -> Validate (Validated.StructInfo ParameterizedFunctor)
validateStruct Resolved.StructInfo {..} = do
  fields <- traverse2 validateParameterizedType _structValues
  pure $ Validated.StructInfo _structParams fields

validateFunctionType
  :: Resolved.FunctionInfo
  -> Validate (Validated.FunctionTypeInfo ParameterizedFunctor)
validateFunctionType info = do
  let Resolved.FunctionType {..} = Resolved._funType info
  baseName <- use currentName
  defLocation <- use currentLocation
  let originalDefinition = WithLocation defLocation info
  attemptedArgs   <- getCompose $ traverse2 (tryNested . validateFunctionArg)       _funArgs
  attemptedReturn <- getCompose $ traverse  (tryNested . validateParameterizedType) _funReturn
  validatedArgs   <- ensure attemptedArgs
  validatedReturn <- fromMaybe (Right UnitType) <$> ensure attemptedReturn
  let validatedFunctionTypeInfo = FunctionTypeInfo
        { _funParams = _funParams
        , _funArgs   = validatedArgs
        , _funReturn = validatedReturn
        }
  if isGeneric info
  then do
    vsFunctions %= M.insert baseName originalDefinition
  else do
    let
      concreteReturn = reifyType @ConcreteFunctor M.empty validatedReturn
      concreteArgs = flip fmap2 validatedArgs \case
        Validated.ByReference innerType -> Validated.ByReference $ reifyType @ConcreteFunctor M.empty innerType
        Validated.ByValue     innerType -> Validated.ByValue     $ reifyType @ConcreteFunctor M.empty innerType
      concreteFunctionTypeInfo = FunctionTypeInfo
        { _funParams = _funParams
        , _funArgs   = concreteArgs
        , _funReturn = concreteReturn
        }
    let request = FunctionInstantiationRequest
          { _firBaseName = baseName
          , _firDefinition = originalDefinition
          , _firFunType = concreteFunctionTypeInfo
          , _firParams = []
          }
    vsInstanceRequests %= (:|> request)
  pure validatedFunctionTypeInfo
  where
    validateFunctionArg = \case
      Resolved.ByReference path -> Validated.ByReference <$> validateParameterizedType path
      Resolved.ByValue     path -> Validated.ByValue     <$> validateParameterizedType path
