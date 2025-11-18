module Lang.Pietre.Stages.Analysis.Validation.Context where

import "this" Prelude

import Control.Lens                                  hiding (mapping, op)
import Control.Monad.Loops                           (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                     (hoistMaybe)
import Data.HashMap.Strict.Extra                     qualified as M
import Data.HashSet                                  qualified as S
import Data.List                                     qualified as L
import Data.Ordered.Set                              qualified as OSet
import Data.Set                                      qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.AST.Common        as Input
import Lang.Pietre.Representations.AST.Resolved      as Input
import Lang.Pietre.Representations.AST.Validated     as Output
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Validation.Expect
import Lang.Pietre.Stages.Analysis.Validation.Monad


retrieveTypeAlias
  :: Monad m
  => BaseName
  -> ValidateT m (Typed Output.TypeAliasInfo)
retrieveTypeAlias =
  retrieveDefinition >=> traverse assertTypeAlias

retrieveConstant
  :: Monad m
  => BaseName
  -> ValidateT m (Typed Output.ConstExpression)
retrieveConstant baseName =
  retrieveDefinition baseName >>= traverse (assertConstant baseName)

retrieveStruct
  :: Monad m
  => BaseName
  -> ValidateT m (StructInfo Validated)
retrieveStruct =
  retrieveDefinition >=> traverse assertStruct

retrieveEnum
  :: Monad m
  => BaseName
  -> ValidateT m [Identifier]
retrieveEnum =
  retrieveDefinition >=> traverse assertEnum

retrieveFunctionType
  :: Monad m
  => BaseName
  -> ValidateT m (FunctionTypeInfo ParameterizedFunctor)
retrieveFunctionType =
  retrieveDefinition >=> traverse assertFunctionType

retrieveFunctionDefinition
  :: Monad m
  => BaseName
  -> ValidateT m Resolved.FunctionInfo
retrieveFunctionDefinition baseName =
  ensure $ asumM
    [ lookupRemoteFunctionDefinition
    , lookupLocalFunctionDefinition
    , throwICE
    ]
  where
    lookupRemoteFunctionDefinition =
      views viFunctions (M.lookup baseName)
    lookupLocalFunctionDefinition baseName =
      uses vsFunctions (M.lookup baseName)
    throwICE = reportICE
      "function definition lookup"
      "function definition not found"
      ["function name: " ++ show baseName]

retrieveTypeParameter
  :: Monad m
  => BaseName
  -> Identifier
  -> ValidateT m Type
retrieveTypeParameter typeName paramName =
  uses contextParams (M.lookup (typeName, paramName)) `onNothingM`
    reportICE
      "type parameter validation"
      "parameter name not found"
      [ "type name: " ++ show typeName
      , "param name: " ++ show paramName
      ]

retrieveStructParams
  :: Monad m
  => BaseName
  -> ValidateT m [Identifier]
retrieveStructParams baseName =
  ensure $ asumM
    [ fmap2 _structParams $ lookupRemoteDefinition
    , fmap2 _structParams $ lookupLocalDefinition
    , fmap (Just . _structParams) $ retrieveInputDefinition
    ]

retrieveDefinition
  :: Monad m
  => BaseName
  -> ValidateT m (Definition Validated)
retrieveDefinition baseName =
  ensure $ asumM
    [ lookupRemoteDefinition baseName
    , lookupLocalDefinition baseName
    , attemptToValidateT baseName
    ]

retrieveVariableType
  :: Monad m
  => Identifier
  -> ValidateT m ConcreteType
retrieveVariableType varName =
  uses currentVariables (M.lookup varName) `onNothing`
    reportICE
      "variable type lookup"
      "variable type not found in scope"
      ["variable name: " ++ show varName]

lookupRemoteDefinition
  :: Monad m
  => BaseName
  -> ValidateT m (Maybe (Definition Validated))
lookupRemoteDefinition baseName =
  views viDefinitions (M.lookup baseName)

lookupLocalDefinition
  :: Monad m
  => BaseName
  -> ValidateT m (Maybe (Definition Validated))
lookupLocalDefinitions baseName =
  uses vsDefinitions (M.lookup baseName)

attemptToValidateT
  :: Monad m
  => BaseName
  -> ValidateT m (Maybe (Definition Validated))
attemptToValidateT baseName =
  validate baseName =<< retrieveInputDefinition baseName

retrieveInputDefinition
  :: Monad m
  => BaseName
  -> ValidateT m (Definition Resolved)
retrieveInputDefinition baseName = do
  views viDefinitions (M.lookup baseName) `onNothingM`
    reportICE "validation definition lookup" "definition not found" ["name: " ++ show baseName]
