{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.AST where

import "this" Prelude

import Control.Lens
import Data.Kind
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Tokens


data ASTPhase = Parsed


class
  ( Show (XTypeAlias p)
  , Show (XEnum      p)
  , Show (XStruct    p)
  , Show (XConst     p)
  , Show (XFunction  p)
  , Show (XIf        p)
  , Show (XFor       p)
  , Show (XWhile     p)
  , Show (XLet       p)
  , Show (XReturn    p)
  , Show (XContinue  p)
  , Show (XBreak     p)
  , Show (XEpression p)
  ) => ASTRepresentation (p :: ASTPhase) where
  type XTypeAlias p :: Type
  type XEnum      p :: Type
  type XStruct    p :: Type
  type XConst     p :: Type
  type XFunction  p :: Type
  type XIf        p :: Type
  type XFor       p :: Type
  type XWhile     p :: Type
  type XLet       p :: Type
  type XReturn    p :: Type
  type XContinue  p :: Type
  type XBreak     p :: Type
  type XEpression p :: Type

instance ASTRepresentation Parsed where
  type XTypeAlias Parsed = Location
  type XEnum      Parsed = Location
  type XStruct    Parsed = Location
  type XConst     Parsed = Location
  type XFunction  Parsed = Location
  type XIf        Parsed = Location
  type XFor       Parsed = Location
  type XWhile     Parsed = Location
  type XLet       Parsed = Location
  type XReturn    Parsed = Location
  type XContinue  Parsed = Location
  type XBreak     Parsed = Location
  type XEpression Parsed = Location


data Module (p :: ASTPhase) = Module
  { _modImports      :: [Import]
  , _modDeclarations :: [Declaration p]
  }

deriving instance ASTRepresentation p => Show (Module p)

instance Semigroup (Module p) where
  Module imports1 decls1 <> Module imports2 decls2 =
    Module (imports1 <> imports2) (decls1 <> decls2)

instance Monoid (Module p) where
  mempty = Module [] []


data Import = Import
  { _importPath :: [Identifier]
  , _importType :: ImportType
  }
  deriving Show


data ImportType
  = Qualified  (Maybe Identifier)
  | Specific   [Identifier]
  | Exhaustive
  deriving Show


data Declaration (p :: ASTPhase)
  = TypeAliasDecl (XTypeAlias p) (TypeAliasInfo p)
  | EnumDecl      (XEnum      p) (EnumInfo      p)
  | StructDecl    (XStruct    p) (StructInfo    p)
  | ConstDecl     (XConst     p) (ConstInfo     p)
  | FunctionDecl  (XFunction  p) (FunctionInfo  p)

deriving instance ASTRepresentation p => Show (Declaration p)


data TypeAliasInfo (p :: ASTPhase) = TypeAliasInfo
  { _aliasName   :: Identifier
  , _aliasParams :: [Identifier]
  , _aliasValue  :: TypeExpr p
  }

deriving instance ASTRepresentation p => Show (TypeAliasInfo p)


data EnumInfo (p :: ASTPhase) = EnumInfo
  { _enumName   :: Identifier
  , _enumValues :: [Identifier]
  }

deriving instance ASTRepresentation p => Show (EnumInfo p)


data StructInfo (p :: ASTPhase) = StructInfo
  { _structName   :: Identifier
  , _structParams :: [Identifier]
  , _structValues :: [(Identifier, TypeExpr p)]
  }

deriving instance ASTRepresentation p => Show (StructInfo p)


data ConstInfo (p :: ASTPhase) = ConstInfo
  { _constName :: Identifier
  , _constType :: TypeExpr p
  , _constExpr :: Expression p
  }

deriving instance ASTRepresentation p => Show (ConstInfo p)


data FunctionInfo (p :: ASTPhase) = FunctionInfo
  { _funName   :: Identifier
  , _funParams :: [Identifier]
  , _funArgs   :: [(Identifier, FunctionArgType p)]
  , _funType   :: Maybe (TypeExpr p)
  , _funBody   :: [Statement p]
  }

deriving instance ASTRepresentation p => Show (FunctionInfo p)


data FunctionArgType (p :: ASTPhase)
  = ByValue     (TypeExpr p)
  | ByReference (TypeExpr p)

deriving instance ASTRepresentation p => Show (FunctionArgType p)


data Statement (p :: ASTPhase)
  = IfStmt         (XIf        p) (IfInfo    p)
  | ForStmt        (XFor       p) (ForInfo   p)
  | WhileStmt      (XWhile     p) (WhileInfo p)
  | LetStmt        (XLet       p) (LetInfo   p)
  | ReturnStmt     (XReturn    p) (Maybe (Expression p))
  | ContinueStmt   (XContinue  p)
  | BreakStmt      (XBreak     p)
  | ExpressionStmt (XEpression p) (Expression p)

deriving instance ASTRepresentation p => Show (Statement p)


data IfInfo (p :: ASTPhase) = IfInfo
  { _ifExpr :: Expression p
  , _ifBody :: [Statement p]
  , _ifElse :: Maybe (ElseInfo p)
  }

deriving instance ASTRepresentation p => Show (IfInfo p)


data ElseInfo (p :: ASTPhase)
  = ElseIf    (IfInfo p)
  | ElseBlock [Statement p]

deriving instance ASTRepresentation p => Show (ElseInfo p)


data ForInfo (p :: ASTPhase) = ForInfo
  { _forVariableName :: Identifier
  , _forRangeExpr    :: Expression p
  , _forBody         :: [Statement p]
  }

deriving instance ASTRepresentation p => Show (ForInfo p)


data WhileInfo (p :: ASTPhase) = WhileInfo
  { _whileExpr :: Expression p
  , _whileBody :: [Statement p]
  }

deriving instance ASTRepresentation p => Show (WhileInfo p)


data LetInfo (p :: ASTPhase) = LetInfo
  { _letName :: Identifier
  , _letType :: Maybe (TypeExpr p)
  , _letExpr :: Expression p
  }

deriving instance ASTRepresentation p => Show (LetInfo p)


data Expression (p :: ASTPhase)

deriving instance ASTRepresentation p => Show (Expression p)


data TypeExpr (p :: ASTPhase)

deriving instance ASTRepresentation p => Show (TypeExpr p)


makeLenses 'Import
