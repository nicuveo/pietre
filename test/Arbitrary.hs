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


instance Arbitrary (Module Parsed) where
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
    [ TypeAliasDecl debugLocation <$> arbitrary
    , EnumDecl      debugLocation <$> arbitrary
    , StructDecl    debugLocation <$> arbitrary
    , ConstDecl     debugLocation <$> arbitrary
    , FunctionDecl  debugLocation <$> arbitrary
    ]
  shrink = \case
    TypeAliasDecl dl info -> TypeAliasDecl dl <$> shrink info
    EnumDecl      dl info -> EnumDecl      dl <$> shrink info
    StructDecl    dl info -> StructDecl    dl <$> shrink info
    ConstDecl     dl info -> ConstDecl     dl <$> shrink info
    FunctionDecl  dl info -> FunctionDecl  dl <$> shrink info

instance Arbitrary (TypeAliasInfo Parsed) where
  arbitrary = liftA3 TypeAliasInfo arbitrary arbitrary arbitrary
  shrink (TypeAliasInfo name params value) = concat
    [ [TypeAliasInfo x    params value | x <- shrink name  ]
    , [TypeAliasInfo name x      value | x <- shrink params]
    , [TypeAliasInfo name params x     | x <- shrink value ]
    ]

instance Arbitrary (EnumInfo Parsed) where
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
      stmt 0 = pure $ BreakStmt debugLocation
      stmt _ = scale (`div` 100) $ oneof
        [ IfStmt         debugLocation <$> arbitrary
        , ForStmt        debugLocation <$> arbitrary
        , WhileStmt      debugLocation <$> arbitrary
        , LetStmt        debugLocation <$> arbitrary
        , ReturnStmt     debugLocation <$> arbitrary
        , ExpressionStmt debugLocation <$> arbitrary
        , pure $ ContinueStmt debugLocation
        , pure $ BreakStmt    debugLocation
        ]
  shrink = \case
    IfStmt         dl info -> IfStmt         dl <$> shrink info
    ForStmt        dl info -> ForStmt        dl <$> shrink info
    WhileStmt      dl info -> WhileStmt      dl <$> shrink info
    LetStmt        dl info -> LetStmt        dl <$> shrink info
    ReturnStmt     dl info -> ReturnStmt     dl <$> shrink info
    ExpressionStmt dl info -> ExpressionStmt dl <$> shrink info
    _ -> []

instance Arbitrary (IfInfo Parsed) where
  arbitrary = liftA3 IfInfo arbitrary arbitrary arbitrary
  shrink (IfInfo iexpr ibody ielse) = concat
    [ [IfInfo x     ibody ielse | x <- shrink iexpr]
    , [IfInfo iexpr x     ielse | x <- shrink ibody]
    , [IfInfo iexpr ibody x     | x <- shrink ielse]
    ]

instance Arbitrary (ElseInfo Parsed) where
  arbitrary = scale (`div` 100) $ oneof
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
      expr _ = oneof
        [ BoolLiteralExpr   debugLocation <$> arbitrary
        , IntLiteralExpr    debugLocation <$> fmap getPositive arbitrary
        , CharLiteralExpr   debugLocation <$> (arbitrary `suchThat` isAsciiLower)
        , StringLiteralExpr debugLocation <$> fmap T.pack (listOf $ arbitrary `suchThat` isAsciiLower)
        ]
--      expr _ = scale (`div` 100) $ oneof
--        [ PathExpr                     debugLocation <$> arbitrary
--        , FieldAccessExpr              debugLocation <$> arbitrary <*> arbitrary
--        , CallExpr                     debugLocation <$> arbitrary <*> arbitrary
--        , ArrayExpr                    debugLocation <$> arbitrary
--        , IndexExpr                    debugLocation <$> arbitrary <*> arbitrary
--        , StructExpr                   debugLocation <$> arbitrary <*> arbitrary
--        , BoolLiteralExpr              debugLocation <$> arbitrary
--        , IntLiteralExpr               debugLocation <$> fmap getPositive arbitrary
--        , CharLiteralExpr              debugLocation <$> (arbitrary `suchThat` isAsciiLower)
--        , StringLiteralExpr            debugLocation <$> fmap T.pack (listOf $ arbitrary `suchThat` isAsciiLower)
--        , ReferenceExpr                debugLocation <$> arbitrary
--        , AdditionExpr                 debugLocation <$> arbitrary <*> arbitrary
--        , SubtractionExpr              debugLocation <$> arbitrary <*> arbitrary
--        , MultiplicationExpr           debugLocation <$> arbitrary <*> arbitrary
--        , DivisionExpr                 debugLocation <$> arbitrary <*> arbitrary
--        , ModuloExpr                   debugLocation <$> arbitrary <*> arbitrary
--        , ExponentiationExpr           debugLocation <$> arbitrary <*> arbitrary
--        , EqualityExpr                 debugLocation <$> arbitrary <*> arbitrary
--        , DifferenceExpr               debugLocation <$> arbitrary <*> arbitrary
--        , GreaterExpr                  debugLocation <$> arbitrary <*> arbitrary
--        , LesserExpr                   debugLocation <$> arbitrary <*> arbitrary
--        , GreaterEqExpr                debugLocation <$> arbitrary <*> arbitrary
--        , LesserEqExpr                 debugLocation <$> arbitrary <*> arbitrary
--        , BoolAndExpr                  debugLocation <$> arbitrary <*> arbitrary
--        , BoolOrExpr                   debugLocation <$> arbitrary <*> arbitrary
--        , CastExpr                     debugLocation <$> arbitrary <*> arbitrary
--        , RangeInclusiveExpr           debugLocation <$> arbitrary <*> arbitrary
--        , RangeExclusiveExpr           debugLocation <$> arbitrary <*> arbitrary
--        , AssignmentExpr               debugLocation <$> arbitrary <*> arbitrary
--        , AdditionAssignmentExpr       debugLocation <$> arbitrary <*> arbitrary
--        , SubtractionAssignmentExpr    debugLocation <$> arbitrary <*> arbitrary
--        , MultiplicationAssignmentExpr debugLocation <$> arbitrary <*> arbitrary
--        , DivisionAssignmentExpr       debugLocation <$> arbitrary <*> arbitrary
--        , ModuloAssignmentExpr         debugLocation <$> arbitrary <*> arbitrary
--        , ExponentiationAssignmentExpr debugLocation <$> arbitrary <*> arbitrary
--        , NegationExpr                 debugLocation <$> arbitrary
--        ]
  shrink = \case
     PathExpr          dl e -> PathExpr          dl <$> shrink e
     ArrayExpr         dl e -> ArrayExpr         dl <$> shrink e
     BoolLiteralExpr   dl e -> BoolLiteralExpr   dl <$> shrink e
     IntLiteralExpr    dl e -> IntLiteralExpr    dl <$> shrink e
     CharLiteralExpr   dl e -> CharLiteralExpr   dl <$> shrink e
     StringLiteralExpr dl e -> StringLiteralExpr dl <$> shrink e
     ReferenceExpr     dl e -> ReferenceExpr     dl <$> shrink e
     NegationExpr      dl e -> NegationExpr      dl <$> shrink e

     FieldAccessExpr              dl e1 e2 ->
       [FieldAccessExpr              dl x e2 | x <- shrink e1] <>
       [FieldAccessExpr              dl e1 x | x <- shrink e2]
     CallExpr                     dl e1 e2 ->
       [CallExpr                     dl x e2 | x <- shrink e1] <>
       [CallExpr                     dl e1 x | x <- shrink e2]
     IndexExpr                    dl e1 e2 ->
       [IndexExpr                    dl x e2 | x <- shrink e1] <>
       [IndexExpr                    dl e1 x | x <- shrink e2]
     StructExpr                   dl e1 e2 ->
       [StructExpr                   dl x e2 | x <- shrink e1] <>
       [StructExpr                   dl e1 x | x <- shrink e2]
     AdditionExpr                 dl e1 e2 ->
       [AdditionExpr                 dl x e2 | x <- shrink e1] <>
       [AdditionExpr                 dl e1 x | x <- shrink e2]
     SubtractionExpr              dl e1 e2 ->
       [SubtractionExpr              dl x e2 | x <- shrink e1] <>
       [SubtractionExpr              dl e1 x | x <- shrink e2]
     MultiplicationExpr           dl e1 e2 ->
       [MultiplicationExpr           dl x e2 | x <- shrink e1] <>
       [MultiplicationExpr           dl e1 x | x <- shrink e2]
     DivisionExpr                 dl e1 e2 ->
       [DivisionExpr                 dl x e2 | x <- shrink e1] <>
       [DivisionExpr                 dl e1 x | x <- shrink e2]
     ModuloExpr                   dl e1 e2 ->
       [ModuloExpr                   dl x e2 | x <- shrink e1] <>
       [ModuloExpr                   dl e1 x | x <- shrink e2]
     ExponentiationExpr           dl e1 e2 ->
       [ExponentiationExpr           dl x e2 | x <- shrink e1] <>
       [ExponentiationExpr           dl e1 x | x <- shrink e2]
     EqualityExpr                 dl e1 e2 ->
       [EqualityExpr                 dl x e2 | x <- shrink e1] <>
       [EqualityExpr                 dl e1 x | x <- shrink e2]
     DifferenceExpr               dl e1 e2 ->
       [DifferenceExpr               dl x e2 | x <- shrink e1] <>
       [DifferenceExpr               dl e1 x | x <- shrink e2]
     GreaterExpr                  dl e1 e2 ->
       [GreaterExpr                  dl x e2 | x <- shrink e1] <>
       [GreaterExpr                  dl e1 x | x <- shrink e2]
     LesserExpr                   dl e1 e2 ->
       [LesserExpr                   dl x e2 | x <- shrink e1] <>
       [LesserExpr                   dl e1 x | x <- shrink e2]
     GreaterEqExpr                dl e1 e2 ->
       [GreaterEqExpr                dl x e2 | x <- shrink e1] <>
       [GreaterEqExpr                dl e1 x | x <- shrink e2]
     LesserEqExpr                 dl e1 e2 ->
       [LesserEqExpr                 dl x e2 | x <- shrink e1] <>
       [LesserEqExpr                 dl e1 x | x <- shrink e2]
     BoolAndExpr                  dl e1 e2 ->
       [BoolAndExpr                  dl x e2 | x <- shrink e1] <>
       [BoolAndExpr                  dl e1 x | x <- shrink e2]
     BoolOrExpr                   dl e1 e2 ->
       [BoolOrExpr                   dl x e2 | x <- shrink e1] <>
       [BoolOrExpr                   dl e1 x | x <- shrink e2]
     CastExpr                     dl e1 e2 ->
       [CastExpr                     dl x e2 | x <- shrink e1] <>
       [CastExpr                     dl e1 x | x <- shrink e2]
     RangeInclusiveExpr           dl e1 e2 ->
       [RangeInclusiveExpr           dl x e2 | x <- shrink e1] <>
       [RangeInclusiveExpr           dl e1 x | x <- shrink e2]
     RangeExclusiveExpr           dl e1 e2 ->
       [RangeExclusiveExpr           dl x e2 | x <- shrink e1] <>
       [RangeExclusiveExpr           dl e1 x | x <- shrink e2]
     AssignmentExpr               dl e1 e2 ->
       [AssignmentExpr               dl x e2 | x <- shrink e1] <>
       [AssignmentExpr               dl e1 x | x <- shrink e2]
     AdditionAssignmentExpr       dl e1 e2 ->
       [AdditionAssignmentExpr       dl x e2 | x <- shrink e1] <>
       [AdditionAssignmentExpr       dl e1 x | x <- shrink e2]
     SubtractionAssignmentExpr    dl e1 e2 ->
       [SubtractionAssignmentExpr    dl x e2 | x <- shrink e1] <>
       [SubtractionAssignmentExpr    dl e1 x | x <- shrink e2]
     MultiplicationAssignmentExpr dl e1 e2 ->
       [MultiplicationAssignmentExpr dl x e2 | x <- shrink e1] <>
       [MultiplicationAssignmentExpr dl e1 x | x <- shrink e2]
     DivisionAssignmentExpr       dl e1 e2 ->
       [DivisionAssignmentExpr       dl x e2 | x <- shrink e1] <>
       [DivisionAssignmentExpr       dl e1 x | x <- shrink e2]
     ModuloAssignmentExpr         dl e1 e2 ->
       [ModuloAssignmentExpr         dl x e2 | x <- shrink e1] <>
       [ModuloAssignmentExpr         dl e1 x | x <- shrink e2]
     ExponentiationAssignmentExpr dl e1 e2 ->
       [ExponentiationAssignmentExpr dl x e2 | x <- shrink e1] <>
       [ExponentiationAssignmentExpr dl e1 x | x <- shrink e2]

instance Arbitrary (PathInfo Parsed) where
  arbitrary = sized path
    where
      path 0 =
        liftA2 PathInfo arbitrary (pure [])
      path _ = scale (`div` 100) $
        liftA2 PathInfo arbitrary arbitrary
  shrink (PathInfo name params) = concat
    [ [PathInfo x    params | x <- shrink name  ]
    , [PathInfo name x      | x <- shrink params]
    ]


debugLocation :: Location
debugLocation = Location "DEBUG" 0 0 0
