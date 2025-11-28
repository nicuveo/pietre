module Lang.Pietre.Stages.Lowering (lowerModule) where

-- import "this" Prelude
--
-- import Control.Lens
-- import Control.Monad.Extra
-- import Data.HashMap.Strict                       qualified as M
-- import GHC.Stack
--
-- import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
-- import Lang.Pietre.Representations.AST.Validated as AST
-- import Lang.Pietre.Representations.Identifier
-- import Lang.Pietre.Representations.Interface
-- import Lang.Pietre.Representations.IR        as IR
-- import Lang.Pietre.Representations.Location
-- import Lang.Pietre.Representations.Name
-- import Lang.Pietre.Stages.Lowering.Collection
-- import Lang.Pietre.Stages.Lowering.Monad

lowerModule :: a
lowerModule = unimplemented

{-

lowerModule
  :: Interface
  -> IR
lowerModule moduleInteface@Interface {..} =
  unimplemented
  -- fmap (lowerFunction moduleInteface) _interfaceSymbols

lowerFunction
  :: Interface
  -> AST.FunctionInfo
  -> IR.Function
lowerFunction interface FunctionInfo {..} =
  runLowering interface do
    sealBlock startLabel
    startBlock startLabel
    for_ (_funArgs _funType) \(argName, argInfo) -> do
      register <- mkRegister =<< forceType (functionArgType argInfo)
      appendArgument startLabel register
      lsRegisters %= M.insert (startLabel, argName) register
    processBlock _funBody
    allBlocks <- use lsBlocks
    -- TODO: filter out unreachable blocks
    pure $ Function startLabel $ collectBlocks allBlocks startLabel

sealBlock
  :: Label
  -> LoweringM ()
sealBlock label = do
  placeholderArgs <- uses lsPlaceholders (M.lookup label)
  void
    $ M.traverseWithKey (registerBlockArgument label)
    $ fold placeholderArgs
  seal label

-- ASSUMPTION: process block MUST be called while already in a block
processBlock
  :: AST.Block Resolved
  -> LoweringM ()
processBlock = \case
  [] -> do
    currentResumeLabel >>= \case
      Just nextLabel ->
        endBlock $ Jump $ mkTarget nextLabel
      Nothing -> do
        -- TODO: emit error if function doesn't return unit type
        endBlock $ Return Nothing
  (stmt:stmts) -> do
    reachable <- isReachable
    if not reachable
    then do
      endBlock Panic
      when (not $ null stmts) $
        -- emit warning: unreachable code
        unimplemented
    else do
      processStatement stmt
      inABlock <- isWithinBlock
      if inABlock
      then do
        processBlock stmts
      else do
        when (not $ null stmts) $
          -- emit warning: unreachable code
          unimplemented

processStatement
  :: AST.Statement Resolved
  -> LoweringM ()
processStatement = \case
  ContinueStmt -> do
    nextLabel <- currentContinueLabel `onNothingM`
      -- TODO: report error instead, remove analysis phase diagnostic
      reportICE "IR lowering" "continue statement not in a loop" []
    endBlock $ Jump $ mkTarget nextLabel
  BreakStmt -> do
    nextLabel <- currentBreakLabel `onNothingM`
      -- TODO: report error instead, remove analysis phase diagnostic
      reportICE "IR lowering" "continue statement not in a loop" []
    endBlock $ Jump $ mkTarget nextLabel
  ReturnStmt typedExpression -> do
    -- when (not $ null stmts) $ emit warning: unreachable code
    register <- traverse forceExpression typedExpression
    endBlock $ Return register
  ExpressionStmt expr -> do
    void $ processExpression expr
  LetStmt LetInfo {..} -> do
    register <- forceExpression _letExpr
    current <- currentBlock
    lsRegisters %= M.insert (current, _letName) register
  IfStmt ifInfo -> do
    resumeLabel <- mkLabel
    withInnerScope resumeLabel $
      processIf resumeLabel ifInfo
    seal resumeLabel
    startBlock resumeLabel
  WhileStmt WhileInfo {..} -> do
    conditionLabel <- mkLabel
    loopBlockLabel <- mkLabel
    resumeLabel    <- mkLabel

    -- finish pre-loop block
    endBlock $ Jump $ mkTarget conditionLabel

    -- condition block
    startBlock conditionLabel
    register <- forceExpression _whileExpr
    endBlock $ Branch
      (mkTarget loopBlockLabel)
      (mkTarget resumeLabel)
      register

    -- loop body block
    seal loopBlockLabel
    withLoop conditionLabel resumeLabel do
      startBlock loopBlockLabel
      processBlock _whileBody

    -- start new outer block
    seal conditionLabel
    seal resumeLabel
    startBlock resumeLabel
  ForStmt _ ->
    unimplemented

processIf
  :: Label
  -> AST.IfInfo Resolved
  -> LoweringM ()
processIf resumeLabel IfInfo {..} = do
  register <- forceExpression _ifExpr
  ifBlockLabel <- mkLabel
  elseBlockLabel <- maybe (pure resumeLabel) (const mkLabel) _ifElse
  endBlock $ Branch
    (mkTarget ifBlockLabel)
    (mkTarget elseBlockLabel)
    register

  seal ifBlockLabel
  startBlock ifBlockLabel
  processBlock _ifBody

  for_ _ifElse \case
    ElseBlock elseBody -> do
      seal elseBlockLabel
      startBlock elseBlockLabel
      processBlock elseBody
    ElseIf ifInfo -> do
      startBlock elseBlockLabel
      processIf resumeLabel ifInfo


forceExpression
  :: TypedExpression
  -> LoweringM Register
forceExpression expr =
  processExpression expr `onNothingM` reportICE
    "IR lowering"
    "process expression did not return a register"
    ["expr: " ++ show expr]

-- ASSUMPTION: always end within a block
processExpression
  :: TypedExpression
  -> LoweringM (Maybe Register)
processExpression TypedExpression {..} = case _exprValue of
  BoolLiteralExpr b -> do
    target <- mkRegister IR.BoolType
    appendInstruction $ AssignB target b
  IntLiteralExpr i -> do
    target <- mkRegister IR.IntType
    appendInstruction $ AssignI target i
  CharLiteralExpr c -> do
    target <- mkRegister IR.CharType
    appendInstruction $ AssignC target c
  StringLiteralExpr _ ->
    unimplemented
  IntNegationExpr e -> do
    previous <- forceExpression e
    target   <- mkRegister IR.IntType
    appendInstruction $ NegateI target previous
  BoolNegationExpr e -> do
    previous <- forceExpression e
    target   <- mkRegister IR.BoolType
    appendInstruction $ NegateB target previous
  AdditionExpr lhs rhs -> do
    -- TODO: handle string concatenation
    basicBinaryOperation Add IR.IntType lhs rhs
  SubtractionExpr lhs rhs ->
    basicBinaryOperation Subtract IR.IntType lhs rhs
  MultiplicationExpr lhs rhs ->
    basicBinaryOperation Multiply IR.IntType lhs rhs
  DivisionExpr lhs rhs ->
    basicBinaryOperation Divide IR.IntType lhs rhs
  ModuloExpr lhs rhs ->
    basicBinaryOperation Modulo IR.IntType lhs rhs
  ExponentiationExpr lhs rhs ->
    basicBinaryOperation Exponent IR.IntType lhs rhs
  EqualityExpr lhs rhs ->
    basicBinaryOperation CmpEQ IR.BoolType lhs rhs
  DifferenceExpr lhs rhs ->
    basicBinaryOperation CmpNE IR.BoolType lhs rhs
  GreaterExpr lhs rhs ->
    basicBinaryOperation CmpGT IR.BoolType lhs rhs
  LesserExpr lhs rhs ->
    basicBinaryOperation CmpLT IR.BoolType lhs rhs
  GreaterEqExpr lhs rhs ->
    basicBinaryOperation CmpGE IR.BoolType lhs rhs
  LesserEqExpr lhs rhs ->
    basicBinaryOperation CmpLE IR.BoolType lhs rhs
  BoolAndExpr lhs rhs -> do
    lhsRegister <- forceExpression lhs
    rhsLabel    <- mkLabel
    resumeLabel <- mkLabel
    endBlock $ Branch (mkTarget rhsLabel) (Target resumeLabel [lhsRegister]) lhsRegister

    seal rhsLabel
    startBlock rhsLabel
    rhsRegister <- forceExpression rhs
    endBlock $ Jump $ Target resumeLabel [rhsRegister]

    seal resumeLabel
    startBlock resumeLabel
    target <- mkRegister IR.BoolType
    appendArgument resumeLabel target
    pure $ Just target
  BoolOrExpr lhs rhs -> do
    lhsRegister <- forceExpression lhs
    rhsLabel    <- mkLabel
    resumeLabel <- mkLabel
    endBlock $ Branch (Target resumeLabel [lhsRegister]) (mkTarget rhsLabel) lhsRegister

    seal rhsLabel
    startBlock rhsLabel
    rhsRegister <- forceExpression rhs
    endBlock $ Jump $ Target resumeLabel [rhsRegister]

    seal resumeLabel
    startBlock resumeLabel
    target <- mkRegister IR.BoolType
    appendArgument resumeLabel target
    pure $ Just target
  CastExpr e t -> do
    previous <- forceExpression e
    target <- mkRegister =<< forceType t
    -- TODO: do we want to insert cast bound checks here?
    appendInstruction $ Cast target previous
  AssignmentExpr lhs rhs -> do
    value <- forceExpression rhs
    assignRegister lhs value
    pure Nothing
  AdditionAssignmentExpr lhs rhs ->
    compoundAssign Add IR.IntType lhs rhs
  SubtractionAssignmentExpr lhs rhs ->
    compoundAssign Subtract IR.IntType lhs rhs
  MultiplicationAssignmentExpr lhs rhs ->
    compoundAssign Multiply IR.IntType lhs rhs
  DivisionAssignmentExpr lhs rhs ->
    compoundAssign Divide IR.IntType lhs rhs
  ModuloAssignmentExpr lhs rhs ->
    compoundAssign Modulo IR.IntType lhs rhs
  ExponentiationAssignmentExpr lhs rhs ->
    compoundAssign Exponent IR.IntType lhs rhs
  FieldAccessExpr structExpr fieldName -> do
    structRegister <- forceExpression structExpr
    (fieldIndex, fieldType) <- retrieveFieldInfo (AST._exprType structExpr) fieldName
    target <- mkRegister =<< forceType fieldType
    appendInstruction $ GetField target structRegister fieldIndex
  ReferenceExpr _ ->
    unimplemented
  PathExpr path ->
    Just <$> resolvePath path
  CallExpr functionPath callArguments -> do
    (functionType, functionValue) <- retrieveFunctionInfo functionPath
    arguments <- traverse forceExpression callArguments
    target <-
      traverse mkRegister . join =<<
      traverse translateType (_funReturn functionType)
    appendInstruction $ case functionValue of
      Left  reg  -> InvokeR target reg  arguments
      Right name -> InvokeN target name arguments
  StructExpr structType fields -> do
    registers <- for fields \(_, e) -> forceExpression e
    target <- mkRegister =<< forceType structType
    appendInstruction $ Combine target registers
  ArrayExpr _ ->
    unimplemented
  IndexExpr _ _ ->
    unimplemented
  RangeInclusiveExpr _lhs _rhs ->
    unimplemented
  RangeExclusiveExpr _lhs _rhs ->
    unimplemented
  where
    compoundAssign instruction regType lhs rhs = do
      value <- basicBinaryOperation instruction regType lhs rhs `onNothingM` reportICE
        "IR lowering"
        "assignment rhs did not yield a valid register"
        []
      assignRegister lhs value
      pure Nothing

    basicBinaryOperation instruction regType lhs rhs = do
      r1 <- forceExpression lhs
      r2 <- forceExpression rhs
      target <- mkRegister regType
      appendInstruction $ instruction target r1 r2

{-


a.b.c.d = x;

%0 <- %a
%1 <- get %0 b
%2 <- get %1 c
%3 <- set %2 d %x
%4 <- set %1 c %3
%5 <- set %0 b %4


-}

assignRegister
  :: TypedExpression
  -> Register
  -> LoweringM ()
assignRegister lhs value = do
  case _exprValue lhs of
    PathExpr path -> do
      let name = case _pathName path of
            LetVariable      n _ -> n
            FunctionArgument n _ -> n
            _                    -> reportICE
              "IR lowering"
              "assignment to a path that is not a local value"
              ["path: " ++ show path]
      label <- currentBlock
      lsRegisters %= M.insert (label, name) value
    FieldAccessExpr _subExpr _field -> do
      unimplemented
    _ -> unimplemented

forceType
  :: HasCallStack
  => PathInfo Resolved
  -> LoweringM IR.Type
forceType path = fromMaybe incorrectTypeError <$> translateType path
  where
    incorrectTypeError =
      reportICE
        "IR lowering"
        "encountered void or unit when looking for a register type"
        ["type info: " ++ show path]

translateType
  :: HasCallStack
  => PathInfo Resolved
  -> LoweringM (Maybe IR.Type)
translateType path@PathInfo {..} =
  case _pathName of
    TopLevelDeclaration name -> translateDeclaration name
    FunctionPointer info     -> translateFunctionType info
    BuiltinFunction _        -> unimplemented
    BuiltinType IntName      -> pure $ Just IR.IntType
    BuiltinType CharName     -> pure $ Just IR.CharType
    BuiltinType BoolName     -> pure $ Just IR.BoolType
    BuiltinType UnitName     -> pure Nothing
    BuiltinType VoidName     -> pure Nothing
    BuiltinType _            -> incorrectTypeError
    TypeParameter _ _        -> incorrectTypeError
    Placeholder              -> incorrectTypeError
    FunctionArgument _ _     -> incorrectTypeError
    LetVariable _ _          -> incorrectTypeError
  where
    translateDeclaration name = do
      definition <- views interfaceDefinitions (M.lookup name)
        `onNothingM` missingTypeDeclarationError name
      case _located definition of
        EnumDef      info -> translateEnumDef info
        StructDef    info -> translateStructDef info
        FunctionDef  info -> translateFunctionType (_funType info)
        TypeAliasDef _    -> incorrectDefinitionError definition
        ConstDef     _    -> incorrectDefinitionError definition

    translateEnumDef EnumInfo {..} =
      pure $ Just $ IR.EnumType $ length _enumValues

    translateStructDef StructInfo {..} = do
      let paramMapping = M.fromList $
            zip _structParams _pathParams
      Just . StructType <$> for _structValues \(_, fieldType) ->
        forceType $ substituteTypes paramMapping fieldType

    translateFunctionType AST.FunctionType {..} = do
      arguments <- traverse (forceType . functionArgType . snd) _funArgs
      result <- traverse translateType _funReturn
      pure $ Just $ IR.FunctionType arguments (join result)

    incorrectTypeError =
      reportICE
        "IR lowering"
        "encountered an incorrect role while expecting a type"
        ["type info: " ++ show path]

    incorrectDefinitionError definition =
      reportICE
        "IR lowering"
        "encountered an incorrect definition while translating a type"
        ["definition: " ++ show definition]

    missingTypeDeclarationError name =
      reportICE
        "IR lowering"
        "could not find definition for top-level type"
        ["type: " ++ show name]

retrieveFunctionInfo
  :: PathInfo Resolved
  -> LoweringM (FunctionType Resolved, Either Register Name)
retrieveFunctionInfo path@PathInfo {..} = do
  case _pathName of
    TopLevelDeclaration name -> handleDeclaration name
    LetVariable name ft      -> handleVariable name ft
    BuiltinFunction _        -> unimplemented
    FunctionArgument _ _     -> unimplemented
    FunctionPointer _        -> incorrectTypeError
    BuiltinType _            -> incorrectTypeError
    TypeParameter _ _        -> incorrectTypeError
    Placeholder              -> incorrectTypeError
  where
    handleDeclaration name = do
      definition <- views interfaceDefinitions (M.lookup name)
        `onNothingM` missingFunctionDeclarationError name
      case _located definition of
        FunctionDef info -> pure (_funType info, Right name)
        _                -> incorrectDefinitionError definition

    handleVariable name varType@PathInfo {..} = do
      case _pathName of
        FunctionPointer ft -> do
          regType <- forceType varType
          register <- resolveName name regType
          pure (ft, Left register)
        _ -> incorrectTypeError

    incorrectTypeError =
      reportICE
        "IR lowering"
        "encountered an incorrect role when retrieving function info"
        ["type info: " ++ show path]

    incorrectDefinitionError definition =
      reportICE
        "IR lowering"
        "encountered an incorrect definition while retrieving function info"
        ["definition: " ++ show definition]

    missingFunctionDeclarationError name =
      reportICE
        "IR lowering"
        "could not find definition for top-level function"
        ["type: " ++ show name]


retrieveFieldInfo
  :: PathInfo Resolved
  -> Identifier
  -> LoweringM (Int, PathInfo Resolved)
retrieveFieldInfo path@PathInfo {..} fieldName = do
  structInfo <- case _pathName of
    TopLevelDeclaration name -> do
      definition <- views interfaceDefinitions (M.lookup name)
        `onNothingM` missingStructDeclarationError name
      case _located definition of
        StructDef info -> pure info
        _              -> incorrectDefinitionError definition
    _ -> incorrectStructError
  pure $ go structInfo 0 $ toList $ _structValues structInfo
  where
    go structInfo _ [] = fieldNotFoundError structInfo
    go structInfo !i ((fn, ft):sfs)
      | fn == fieldName = (i, substituteTypes (getMapping structInfo) ft)
      | otherwise       = go structInfo (i+1) sfs

    getMapping StructInfo {..} =
      M.fromList $ zip _structParams _pathParams

    fieldNotFoundError structInfo =
      reportICE
        "IR lowering"
        "struct field not found in struct definition"
        [ "field name: " ++ show fieldName
        , "struct info: " ++ show structInfo
        ]

    incorrectStructError =
      reportICE
        "IR lowering"
        "encountered an incorrect role when retrieving struct info"
        ["type info: " ++ show path]

    incorrectDefinitionError definition =
      reportICE
        "IR lowering"
        "encountered an incorrect definition while retrieving a struct info"
        ["definition: " ++ show definition]

    missingStructDeclarationError name =
      reportICE
        "IR lowering"
        "could not find definition for top-level type"
        ["type: " ++ show name]


resolvePath
  :: PathInfo Resolved
  -> LoweringM Register
resolvePath path@PathInfo {..} =
  case _pathName of
    TopLevelDeclaration name   -> handleDeclaration name
    LetVariable name varT      -> resolveName name =<< forceType varT
    FunctionArgument name argT -> resolveName name =<< forceType (functionArgType argT)
    BuiltinFunction _          -> unimplemented
    FunctionPointer _          -> incorrectTypeError
    BuiltinType _              -> incorrectTypeError
    TypeParameter _ _          -> incorrectTypeError
    Placeholder                -> incorrectTypeError
  where
    handleDeclaration name = do
      target <- mkRegister =<< forceType path
      void $ appendInstruction $ AssignA target name
      pure target

    incorrectTypeError =
      reportICE
        "IR lowering"
        "encountered an incorrect role when resolving path"
        ["type info: " ++ show path]

resolveName
  :: Identifier
  -> IR.Type
  -> LoweringM Register
resolveName name regType = do
  label <- currentBlock
  resolveNameIn label name regType

resolveNameIn
  :: Label
  -> Identifier
  -> IR.Type
  -> LoweringM Register
resolveNameIn label name regType =
  uses lsRegisters (M.lookup (label, name)) `onNothingM` do
    ifM (isSealed label)
      searchRecursively
      introduceTempRegister
  where
    searchRecursively = do
      ps <- parents label
      case ps of
        [parent] ->
          resolveNameIn parent name regType
        _ -> do
          blockArgument <- mkRegister regType
          registerBlockArgument label name blockArgument
          pure blockArgument

    introduceTempRegister =
      uses lsPlaceholders (M.lookup label >=> M.lookup name) `onNothingM` do
        tempRegister <- mkRegister regType
        lsPlaceholders %= M.insertWith
          M.union
          label
          (M.singleton name tempRegister)
        pure tempRegister

registerBlockArgument
  :: Label
  -> Identifier
  -> Register
  -> LoweringM ()
registerBlockArgument label name blockArgument = do
  ps <- parents label
  appendArgument label blockArgument
  lsRegisters %= M.insert (label, name) blockArgument
  for_ ps \parent -> do
    source <- resolveNameIn parent name (_registerType blockArgument)
    blockInfo parent . blockTerminator %= appendTerminatorArg source
  where
    appendTerminatorArg source = \case
      Jump target -> Jump (target & tgtArgs <>~ [source])
      Branch tgt1 tgt2 r
        | label == _tgtLabel tgt1 -> Branch (tgt1 & tgtArgs <>~ [source]) tgt2 r
        | otherwise               -> Branch tgt1 (tgt2 & tgtArgs <>~ [source]) r
      terminator -> reportICE
        "IR lowering"
        "parent of a block does not point back to it"
        [ "parent terminator: " ++ show terminator
        , "child label: " ++ show label
        ]

-}
