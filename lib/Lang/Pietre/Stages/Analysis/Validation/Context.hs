module Lang.Pietre.Stages.Analysis.Validation.Context where

import                "this" Prelude

import                Control.Lens                                  hiding
                                                                    (mapping,
                                                                     op)
import                Data.HashMap.Strict.Extra                     qualified as M

import                Lang.Pietre.Internal.Diagnosis
import                Lang.Pietre.Internal.ICE
import                Lang.Pietre.Representations.AST.Resolved      qualified as Resolved
import                Lang.Pietre.Representations.AST.Validated
import                Lang.Pietre.Representations.Identifier
import                Lang.Pietre.Representations.Location
import                Lang.Pietre.Representations.Name
import {-# SOURCE #-} Lang.Pietre.Stages.Analysis.Validation
import                Lang.Pietre.Stages.Analysis.Validation.Expect
import                Lang.Pietre.Stages.Analysis.Validation.Monad


retrieveTypeAlias
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m TypeAliasInfo
retrieveTypeAlias =
  retrieveDefinition >=> assertTypeAlias

retrieveConstant
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m (Typed ConstExpression)
retrieveConstant baseName =
  retrieveDefinition baseName >>= assertConstant baseName

retrieveStruct
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m (StructInfo ParameterizedFunctor)
retrieveStruct =
  retrieveDefinition >=> assertStruct

retrieveEnum
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m [Identifier]
retrieveEnum =
  retrieveDefinition >=> assertEnum

retrieveFunctionType
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m (FunctionTypeInfo ParameterizedFunctor)
retrieveFunctionType =
  retrieveDefinition >=> assertFunctionType

retrieveFunctionDefinition
  :: (HasCallStack, MonadDiagnosis m)
  => BaseName
  -> ValidateT m (WithLocation Resolved.FunctionInfo)
retrieveFunctionDefinition baseName =
  ensure =<< asumM
    [ lookupRemoteFunctionDefinition
    , lookupLocalFunctionDefinition
    , throwICE
    ]
  where
    lookupRemoteFunctionDefinition =
      views viFunctions (M.lookup baseName)
    lookupLocalFunctionDefinition =
      uses vsFunctions (M.lookup baseName)
    throwICE = reportICE
      "function definition lookup"
      "function definition not found"
      ["function name: " ++ show baseName]

retrieveTypeParameter
  :: (HasCallStack, MonadDiagnosis m)
  => BaseName
  -> Identifier
  -> ValidateT m ConcreteType
retrieveTypeParameter typeBaseName paramName =
  uses currentParams (M.lookup (typeBaseName, paramName)) `onNothingM`
    reportICE
      "type parameter validation"
      "parameter name not found"
      [ "type name: " ++ show typeBaseName
      , "param name: " ++ show paramName
      ]

retrieveStructParams
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m [Identifier]
retrieveStructParams baseName =
  ensure =<< asumM
    [ fmap2 _structParams $
        traverse assertStruct =<< lookupRemoteDefinition baseName
    , fmap2 _structParams $
        traverse assertStruct =<< lookupLocalDefinition baseName
    , fmap (Just . Resolved._structParams) $
        assertStructDefinition . _located =<< retrieveInputDefinition baseName
    ]

retrieveDefinition
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m Definition
retrieveDefinition baseName =
  ensure =<< asumM
    [ lookupRemoteDefinition baseName
    , lookupLocalDefinition baseName
    , attemptToValidate baseName
    ]

retrieveVariableType
  :: (HasCallStack, MonadDiagnosis m)
  => Identifier
  -> ValidateT m ConcreteType
retrieveVariableType varName =
  uses currentVariables (M.lookup varName) `onNothingM`
    reportICE
      "variable type lookup"
      "variable type not found in scope"
      ["variable name: " ++ show varName]

lookupRemoteDefinition
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m (Maybe Definition)
lookupRemoteDefinition baseName =
  views viDefinitions (M.lookup baseName)

lookupLocalDefinition
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m (Maybe Definition)
lookupLocalDefinition baseName =
  uses vsDefinitions (M.lookup baseName)

attemptToValidate
  :: MonadDiagnosis m
  => BaseName
  -> ValidateT m (Maybe Definition)
attemptToValidate baseName =
  validateDefinition baseName =<< retrieveInputDefinition baseName

retrieveInputDefinition
  :: (HasCallStack, MonadDiagnosis m)
  => BaseName
  -> ValidateT m (WithLocation Resolved.Definition)
retrieveInputDefinition baseName = do
  views viLocalDefinitions (M.lookup baseName) `onNothingM`
    reportICE "validation definition lookup" "definition not found" ["name: " ++ show baseName]
