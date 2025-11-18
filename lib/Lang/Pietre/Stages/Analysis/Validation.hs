module Lang.Pietre.Stages.Analysis.Validation (validate) where

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
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Resolved  as Resolved
import Lang.Pietre.Representations.AST.Validated as Validated
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Monad
import Lang.Pietre.Stages.Analysis.Validation.Expr


validate
  :: MonadDiagnosis m
  => BaseName
  -> WithLocation Resolved.Definition
  -> ValidateT m (Maybe Validated.Definition)
validate baseName definition = do
  alreadyValidated <- uses vsValidated $ S.member name
  if alreadyValidated
  then lookupLocalDefinition baseName
  else do
    defStack <- use vsDefinitionStack
    if name `OSet.member` defStack
    then report $ ErrorCyclicDefinition (snd $ L.break (== name) $ OSet.toList defStack) name
    else do
      vsDefinitionStack %= (baseName OSet.<|)
      result <- try $
        withContext baseName (_location definition) $
          case _located definition of
            TypeAliasDef info -> validateTypeAlias info
            StructDef    info -> validateStruct info
            EnumDef      info -> pure $ EnumDef info
            ConstDef     info -> validateConst info
            FunctionDef  info -> validateFunctionType info
      vsDefinitionStack %= OSet.delete baseName
      vsValidated %= S.insert baseName
      whenJust result \definition -> do
        vsDefinitions %= M.insert baseName definition
      pure result

validateTypeAlias
  :: MonadDiagnosis m
  => TypeAliasInfo Resolved
  -> ValidateT m Validated.Definition
validateTypeAlias TypeAliasInfo {..} = do
  validatedValue <- validateParameterizedType _aliasValue
  pure $ TypeAliasDef $ Output.TypeAliasInfo _aliasParams validatedValue

validateConst
  :: MonadDiagnosis m
  => ConstInfo Resolved
  -> ValidateT m Validated.Definition
validateConst ConstInfo {..} = do
  attemptedType <- try $ validateType _constType
  attemptedExpr <- try $ validateConstExpression _constExpr
  validatedType <- ensure attemptedType
  validatedExpr <- ensure attemptedExpr
  expectType validatedType (_typeInfo resolvedExpr)
  pure $ ConstDef $ validatedExpr

validateStruct
  :: MonadDiagnosis m
  => Input.StructInfo Resolved
  -> ValidateT m Validated.Definition
validateStruct StructInfo {..} =
  fields <- traverse2 validateParameterizedType _structValues
  pure $ StructDef $ StructInfo _structParams fields

validateFunctionType
  :: MonadDiagnosis m
  => FunctionInfo Resolved
  -> ValidateT m Validated.Definition
validateFunctionType info = do
  let FunctionType {..} = _funType info
  attemptedArgs   <- getCompose (traverse2 (tryNested . validateFunctionArg)       _funArgs)
  attemptedReturn <- getCompose (traverse  (tryNested . validateParameterizedType) _funReturn)
  validatedFunctionTypeInfo <- liftA2
    (FunctionTypeInfo _funParams)
    (ensure attemptedArgs)
    (ensure attemptedReturn)
  unless (isGeneric info) do
    baseName <- currentName
    let request = FunctionInstantiationRequest
          { _firBaseName = baseName
          , _firDefinition = info
          , _firFunType = validatedFunctionTypeInfo
          , _firParams = []
          }
    vsInstanceRequests %= (:|> request)
  pure $ FunctionDef validatedFunctionTypeInfo
  where
    validateFunctionArg = \case
      ByReference path -> ByReference <$> validatedParameterizedType path
      ByValue     path -> ByValue     <$> validatedParameterizedType path
