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
  :: BaseName
  -> Validate TypeAliasInfo
retrieveTypeAlias =
  retrieveDefinition >=> assertTypeAlias

retrieveConstant
  :: BaseName
  -> Validate (Typed ConstExpression)
retrieveConstant baseName =
  retrieveDefinition baseName >>= assertConstant baseName

retrieveStruct
  :: BaseName
  -> Validate (StructInfo ParameterizedFunctor)
retrieveStruct =
  retrieveDefinition >=> assertStruct

retrieveEnum
  :: BaseName
  -> Validate [Identifier]
retrieveEnum =
  retrieveDefinition >=> assertEnum

retrieveFunctionType
  :: BaseName
  -> Validate (FunctionTypeInfo ParameterizedFunctor)
retrieveFunctionType =
  retrieveDefinition >=> assertFunctionType

retrieveFunctionDefinition
  :: HasCallStack
  => BaseName
  -> Validate (WithLocation Resolved.FunctionInfo)
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
  :: HasCallStack
  => BaseName
  -> Identifier
  -> Validate ConcreteType
retrieveTypeParameter typeBaseName paramName =
  uses currentParams (M.lookup (typeBaseName, paramName)) `onNothingM`
    reportICE
      "type parameter validation"
      "parameter name not found"
      [ "type name: " ++ show typeBaseName
      , "param name: " ++ show paramName
      ]

retrieveStructParams
  :: BaseName
  -> Validate [Identifier]
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
  :: BaseName
  -> Validate Definition
retrieveDefinition baseName =
  ensure =<< asumM
    [ lookupRemoteDefinition baseName
    , lookupLocalDefinition baseName
    , attemptToValidate baseName
    ]

retrieveVariableType
  :: HasCallStack
  => Identifier
  -> Validate ConcreteType
retrieveVariableType varName =
  uses currentVariables (M.lookup varName) `onNothingM`
    reportICE
      "variable type lookup"
      "variable type not found in scope"
      ["variable name: " ++ show varName]

lookupRemoteDefinition
  :: BaseName
  -> Validate (Maybe Definition)
lookupRemoteDefinition baseName =
  views viDefinitions (M.lookup baseName)

lookupLocalDefinition
  :: BaseName
  -> Validate (Maybe Definition)
lookupLocalDefinition baseName =
  uses vsDefinitions (M.lookup baseName)

attemptToValidate
  :: BaseName
  -> Validate (Maybe Definition)
attemptToValidate baseName =
  validateDefinition baseName =<< retrieveInputDefinition baseName

retrieveInputDefinition
  :: HasCallStack
  => BaseName
  -> Validate (WithLocation Resolved.Definition)
retrieveInputDefinition baseName = do
  views viLocalDefinitions (M.lookup baseName) `onNothingM`
    reportICE "validation definition lookup" "definition not found" ["name: " ++ show baseName]
