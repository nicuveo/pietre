{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import "this" Prelude

import Control.Applicative                  (liftA3)
import Data.Char                            (isAsciiLower, isLetter)
import Data.List.NonEmpty                   qualified as NE
import Data.Text                            qualified as T
import Lang.Pietre
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens
import Test.Tasty.QuickCheck


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> listOf1 arbitrary
  shrink = mapMaybe NE.nonEmpty . shrink . NE.toList


instance Arbitrary a => Arbitrary (WithLocation a) where
  arbitrary = WithLocation (Location "DEBUG" 0 0 0) <$> arbitrary
  shrink (WithLocation l a) = map (WithLocation l) $ shrink a


instance Arbitrary Module where
  arbitrary = resize 2 $ liftA2 Module arbitrary arbitrary
  shrink (Module imports decls) = concat
    [ [Module x       decls | x <- shrink imports]
    , [Module imports x     | x <- shrink decls  ]
    ]

instance Arbitrary Import where
  arbitrary = liftA2 Import arbitrary arbitrary
  shrink (Import ipath itype) = concat
    [ [Import x     itype | x <- shrink ipath]
    , [Import ipath x     | x <- shrink itype]
    ]

instance Arbitrary Identifier where
  arbitrary = fmap (T.pack . ('_':)) $ listOf $ arbitrary `suchThat` isLetter
  shrink i
    | T.length i == 1 = []
    | otherwise       = [T.init i]

instance Arbitrary ImportType where
  arbitrary = oneof
    [ Qualified <$> arbitrary
    , Specific  <$> arbitrary
    , pure Exhaustive
    ]
  shrink = \case
    Qualified mid -> Qualified <$> shrink mid
    Specific  ids -> Specific  <$> shrink ids
    Exhaustive    -> []

instance Arbitrary (Declaration Parsed) where
  arbitrary = oneof
    [ TypeAliasDecl <$> arbitrary
    , EnumDecl      <$> arbitrary
    , StructDecl    <$> arbitrary
    , ConstDecl     <$> arbitrary
    , FunctionDecl  <$> arbitrary
    ]
  shrink = \case
    TypeAliasDecl info -> TypeAliasDecl <$> shrink info
    EnumDecl      info -> EnumDecl      <$> shrink info
    StructDecl    info -> StructDecl    <$> shrink info
    ConstDecl     info -> ConstDecl     <$> shrink info
    FunctionDecl  info -> FunctionDecl  <$> shrink info

instance Arbitrary (TypeAliasInfo Parsed) where
  arbitrary = liftA3 TypeAliasInfo arbitrary arbitrary arbitrary
  shrink (TypeAliasInfo name params value) = concat
    [ [TypeAliasInfo x    params value | x <- shrink name  ]
    , [TypeAliasInfo name x      value | x <- shrink params]
    , [TypeAliasInfo name params x     | x <- shrink value ]
    ]

instance Arbitrary EnumInfo where
  arbitrary = liftA2 EnumInfo arbitrary arbitrary
  shrink (EnumInfo name values) = concat
    [ EnumInfo
      <$> shrink name
      <*> pure   values
    , EnumInfo
      <$> pure   name
      <*> shrink values
    ]

instance Arbitrary (StructInfo Parsed) where
  arbitrary = liftA3 StructInfo arbitrary arbitrary arbitrary
  shrink (StructInfo name params values) = concat
    [ [StructInfo x    params values | x <- shrink name  ]
    , [StructInfo name x      values | x <- shrink params]
    , [StructInfo name params x      | x <- shrink values]
    ]

instance Arbitrary (ConstInfo Parsed) where
  arbitrary = liftA3 ConstInfo arbitrary arbitrary arbitrary
  shrink (ConstInfo name ctype expr) = concat
    [ [ConstInfo x    ctype expr | x <- shrink name ]
    , [ConstInfo name x     expr | x <- shrink ctype]
    , [ConstInfo name ctype x    | x <- shrink expr ]
    ]

instance Arbitrary (FunctionInfo Parsed) where
  arbitrary = FunctionInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink (FunctionInfo name params args rtype body) = concat
    [ [FunctionInfo x    params args rtype body | x <- shrink name  ]
    , [FunctionInfo name x      args rtype body | x <- shrink params]
    , [FunctionInfo name params x    rtype body | x <- shrink args  ]
    , [FunctionInfo name params args x     body | x <- shrink rtype ]
    , [FunctionInfo name params args rtype x    | x <- shrink body  ]
    ]

instance Arbitrary (FunctionArgType Parsed) where
  arbitrary = oneof
    [ ByValue     <$> arbitrary
    , ByReference <$> arbitrary
    ]
  shrink = \case
    ByValue     te -> ByValue     <$> shrink te
    ByReference te -> ByReference <$> shrink te

instance Arbitrary (Statement Parsed) where
  arbitrary = sized stmt
    where
      stmt 0 = pure BreakStmt
      stmt _ = scale (`div` 2) $ oneof
        [ IfStmt         <$> arbitrary
        , ForStmt        <$> arbitrary
        , WhileStmt      <$> arbitrary
        , LetStmt        <$> arbitrary
        , ReturnStmt     <$> arbitrary
        , ExpressionStmt <$> arbitrary
        , pure ContinueStmt
        , pure BreakStmt
        ]
  shrink = \case
    IfStmt         info -> IfStmt         <$> shrink info
    ForStmt        info -> ForStmt        <$> shrink info
    WhileStmt      info -> WhileStmt      <$> shrink info
    LetStmt        info -> LetStmt        <$> shrink info
    ReturnStmt     info -> ReturnStmt     <$> shrink info
    ExpressionStmt info -> ExpressionStmt <$> shrink info
    _ -> []

instance Arbitrary (IfInfo Parsed) where
  arbitrary = liftA3 IfInfo arbitrary arbitrary arbitrary
  shrink (IfInfo iexpr ibody ielse) = concat
    [ [IfInfo x     ibody ielse | x <- shrink iexpr]
    , [IfInfo iexpr x     ielse | x <- shrink ibody]
    , [IfInfo iexpr ibody x     | x <- shrink ielse]
    ]

instance Arbitrary (ElseInfo Parsed) where
  arbitrary = scale (`div` 2) $ oneof
    [ ElseIf    <$> arbitrary
    , ElseBlock <$> arbitrary
    ]
  shrink = \case
    ElseIf    ii    -> ElseIf    <$> shrink ii
    ElseBlock stmts -> ElseBlock <$> shrink stmts

instance Arbitrary (ForInfo Parsed) where
  arbitrary = liftA3 ForInfo arbitrary arbitrary arbitrary
  shrink (ForInfo name expr body) = concat
    [ [ForInfo x    expr body | x <- shrink name]
    , [ForInfo name x    body | x <- shrink expr]
    , [ForInfo name expr x    | x <- shrink body]
    ]

instance Arbitrary (WhileInfo Parsed) where
  arbitrary = liftA2 WhileInfo arbitrary arbitrary
  shrink (WhileInfo expr body) = concat
    [ [WhileInfo x    body | x <- shrink expr]
    , [WhileInfo expr x    | x <- shrink body]
    ]

instance Arbitrary (LetInfo Parsed) where
  arbitrary = liftA3 LetInfo arbitrary arbitrary arbitrary
  shrink (LetInfo name ltype expr) = concat
    [ [LetInfo x    ltype expr | x <- shrink name ]
    , [LetInfo name x     expr | x <- shrink ltype]
    , [LetInfo name ltype x    | x <- shrink expr ]
    ]

instance Arbitrary (Expression Parsed) where
  arbitrary = sized expr
    where
      expr 0 = oneof
        [ BoolLiteralExpr   <$> arbitrary
        , IntLiteralExpr    <$> fmap getPositive arbitrary
        , CharLiteralExpr   <$> (arbitrary `suchThat` isAsciiLower)
        , StringLiteralExpr <$> fmap T.pack (listOf $ arbitrary `suchThat` isAsciiLower)
        ]
      expr _ = scale (`div` 2) $ oneof
        [ PathExpr                     <$> arbitrary
        , FieldAccessExpr              <$> arbitrary <*> arbitrary
        , CallExpr                     <$> arbitrary <*> arbitrary
        , ArrayExpr                    <$> arbitrary
        , IndexExpr                    <$> arbitrary <*> arbitrary
        , StructExpr                   <$> arbitrary <*> arbitrary
        , BoolLiteralExpr              <$> arbitrary
        , IntLiteralExpr               <$> fmap getPositive arbitrary
        , CharLiteralExpr              <$> (arbitrary `suchThat` isAsciiLower)
        , StringLiteralExpr            <$> fmap T.pack (listOf $ arbitrary `suchThat` isAsciiLower)
        , ReferenceExpr                <$> arbitrary
        , AdditionExpr                 <$> arbitrary <*> arbitrary
        , SubtractionExpr              <$> arbitrary <*> arbitrary
        , MultiplicationExpr           <$> arbitrary <*> arbitrary
        , DivisionExpr                 <$> arbitrary <*> arbitrary
        , ModuloExpr                   <$> arbitrary <*> arbitrary
        , ExponentiationExpr           <$> arbitrary <*> arbitrary
        , EqualityExpr                 <$> arbitrary <*> arbitrary
        , DifferenceExpr               <$> arbitrary <*> arbitrary
        , GreaterExpr                  <$> arbitrary <*> arbitrary
        , LesserExpr                   <$> arbitrary <*> arbitrary
        , GreaterEqExpr                <$> arbitrary <*> arbitrary
        , LesserEqExpr                 <$> arbitrary <*> arbitrary
        , BoolAndExpr                  <$> arbitrary <*> arbitrary
        , BoolOrExpr                   <$> arbitrary <*> arbitrary
        , CastExpr                     <$> arbitrary <*> arbitrary
        , RangeInclusiveExpr           <$> arbitrary <*> arbitrary
        , RangeExclusiveExpr           <$> arbitrary <*> arbitrary
        , AssignmentExpr               <$> arbitrary <*> arbitrary
        , AdditionAssignmentExpr       <$> arbitrary <*> arbitrary
        , SubtractionAssignmentExpr    <$> arbitrary <*> arbitrary
        , MultiplicationAssignmentExpr <$> arbitrary <*> arbitrary
        , DivisionAssignmentExpr       <$> arbitrary <*> arbitrary
        , ModuloAssignmentExpr         <$> arbitrary <*> arbitrary
        , ExponentiationAssignmentExpr <$> arbitrary <*> arbitrary
        , IntNegationExpr              <$> arbitrary
        , BoolNegationExpr             <$> arbitrary
        ]
  shrink = \case
     PathExpr          e -> PathExpr          <$> shrink e
     ArrayExpr         e -> ArrayExpr         <$> shrink e
     BoolLiteralExpr   e -> BoolLiteralExpr   <$> shrink e
     IntLiteralExpr    e -> IntLiteralExpr    <$> shrink e
     CharLiteralExpr   e -> CharLiteralExpr   <$> shrink e
     StringLiteralExpr e -> StringLiteralExpr <$> shrink e
     ReferenceExpr     e -> ReferenceExpr     <$> shrink e
     IntNegationExpr   e -> IntNegationExpr   <$> shrink e
     BoolNegationExpr  e -> BoolNegationExpr  <$> shrink e

     FieldAccessExpr              e1 e2 ->
       [FieldAccessExpr              x e2 | x <- shrink e1] <>
       [FieldAccessExpr              e1 x | x <- shrink e2]
     CallExpr                     e1 e2 ->
       [CallExpr                     x e2 | x <- shrink e1] <>
       [CallExpr                     e1 x | x <- shrink e2]
     IndexExpr                    e1 e2 ->
       [IndexExpr                    x e2 | x <- shrink e1] <>
       [IndexExpr                    e1 x | x <- shrink e2]
     StructExpr                   e1 e2 ->
       [StructExpr                   x e2 | x <- shrink e1] <>
       [StructExpr                   e1 x | x <- shrink e2]
     AdditionExpr                 e1 e2 ->
       [AdditionExpr                 x e2 | x <- shrink e1] <>
       [AdditionExpr                 e1 x | x <- shrink e2]
     SubtractionExpr              e1 e2 ->
       [SubtractionExpr              x e2 | x <- shrink e1] <>
       [SubtractionExpr              e1 x | x <- shrink e2]
     MultiplicationExpr           e1 e2 ->
       [MultiplicationExpr           x e2 | x <- shrink e1] <>
       [MultiplicationExpr           e1 x | x <- shrink e2]
     DivisionExpr                 e1 e2 ->
       [DivisionExpr                 x e2 | x <- shrink e1] <>
       [DivisionExpr                 e1 x | x <- shrink e2]
     ModuloExpr                   e1 e2 ->
       [ModuloExpr                   x e2 | x <- shrink e1] <>
       [ModuloExpr                   e1 x | x <- shrink e2]
     ExponentiationExpr           e1 e2 ->
       [ExponentiationExpr           x e2 | x <- shrink e1] <>
       [ExponentiationExpr           e1 x | x <- shrink e2]
     EqualityExpr                 e1 e2 ->
       [EqualityExpr                 x e2 | x <- shrink e1] <>
       [EqualityExpr                 e1 x | x <- shrink e2]
     DifferenceExpr               e1 e2 ->
       [DifferenceExpr               x e2 | x <- shrink e1] <>
       [DifferenceExpr               e1 x | x <- shrink e2]
     GreaterExpr                  e1 e2 ->
       [GreaterExpr                  x e2 | x <- shrink e1] <>
       [GreaterExpr                  e1 x | x <- shrink e2]
     LesserExpr                   e1 e2 ->
       [LesserExpr                   x e2 | x <- shrink e1] <>
       [LesserExpr                   e1 x | x <- shrink e2]
     GreaterEqExpr                e1 e2 ->
       [GreaterEqExpr                x e2 | x <- shrink e1] <>
       [GreaterEqExpr                e1 x | x <- shrink e2]
     LesserEqExpr                 e1 e2 ->
       [LesserEqExpr                 x e2 | x <- shrink e1] <>
       [LesserEqExpr                 e1 x | x <- shrink e2]
     BoolAndExpr                  e1 e2 ->
       [BoolAndExpr                  x e2 | x <- shrink e1] <>
       [BoolAndExpr                  e1 x | x <- shrink e2]
     BoolOrExpr                   e1 e2 ->
       [BoolOrExpr                   x e2 | x <- shrink e1] <>
       [BoolOrExpr                   e1 x | x <- shrink e2]
     CastExpr                     e1 e2 ->
       [CastExpr                     x e2 | x <- shrink e1] <>
       [CastExpr                     e1 x | x <- shrink e2]
     RangeInclusiveExpr           e1 e2 ->
       [RangeInclusiveExpr           x e2 | x <- shrink e1] <>
       [RangeInclusiveExpr           e1 x | x <- shrink e2]
     RangeExclusiveExpr           e1 e2 ->
       [RangeExclusiveExpr           x e2 | x <- shrink e1] <>
       [RangeExclusiveExpr           e1 x | x <- shrink e2]
     AssignmentExpr               e1 e2 ->
       [AssignmentExpr               x e2 | x <- shrink e1] <>
       [AssignmentExpr               e1 x | x <- shrink e2]
     AdditionAssignmentExpr       e1 e2 ->
       [AdditionAssignmentExpr       x e2 | x <- shrink e1] <>
       [AdditionAssignmentExpr       e1 x | x <- shrink e2]
     SubtractionAssignmentExpr    e1 e2 ->
       [SubtractionAssignmentExpr    x e2 | x <- shrink e1] <>
       [SubtractionAssignmentExpr    e1 x | x <- shrink e2]
     MultiplicationAssignmentExpr e1 e2 ->
       [MultiplicationAssignmentExpr x e2 | x <- shrink e1] <>
       [MultiplicationAssignmentExpr e1 x | x <- shrink e2]
     DivisionAssignmentExpr       e1 e2 ->
       [DivisionAssignmentExpr       x e2 | x <- shrink e1] <>
       [DivisionAssignmentExpr       e1 x | x <- shrink e2]
     ModuloAssignmentExpr         e1 e2 ->
       [ModuloAssignmentExpr         x e2 | x <- shrink e1] <>
       [ModuloAssignmentExpr         e1 x | x <- shrink e2]
     ExponentiationAssignmentExpr e1 e2 ->
       [ExponentiationAssignmentExpr x e2 | x <- shrink e1] <>
       [ExponentiationAssignmentExpr e1 x | x <- shrink e2]

instance Arbitrary (PathInfo Parsed) where
  arbitrary = sized path
    where
      path 0 =
        liftA2 PathInfo arbitrary (pure [])
      path _ = scale (`div` 2) $
        liftA2 PathInfo arbitrary arbitrary
  shrink (PathInfo name params) = concat
    [ [PathInfo x    params | x <- shrink name  ]
    , [PathInfo name x      | x <- shrink params]
    ]
