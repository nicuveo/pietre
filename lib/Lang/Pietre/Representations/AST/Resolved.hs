module Lang.Pietre.Representations.AST.Resolved where

import "this" Prelude

import Control.Lens
import Data.Kind
import Data.List.NonEmpty                     qualified as NE
import Prettyprinter
import Prettyprinter.Render.Text

import Lang.Pietre.Representations.AST.Common (ASTPhase (Resolved))
import Lang.Pietre.Representations.AST.Common qualified as Common
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Location


--------------------------------------------------------------------------------
-- AST Representation

instance ASTRepresentation Resolved where
  type PathBodyType   Resolved = Role
  type ExpressionType Resolved = WithLocation (Common.Expression Resolved)
  type ForInfoType    Resolved = Common.ForInfo Resolved
  type LetInfoType    Resolved = Common.LetInfo Resolved


--------------------------------------------------------------------------------
-- Resolved AST definitions

data Role
  = BuiltinType BaseName
  | BuiltinFunction BaseName
  | Struct BaseName
  | Enum BaseName
  | Constant BaseName
  | Function BaseName
  | TypeAlias BaseName
  | TypeParameter BaseName Identifier
  | Placeholder
  -- | FunctionPointer (Common.FunctionType Resolved)
  | FunctionArgument Identifier (Common.FunctionArgType Resolved)
  | LetVariable Identifier (Maybe (Common.PathInfo Resolved))
  deriving (Show, Eq, Ord, Generic)

instance Hashable Role


--------------------------------------------------------------------------------
-- Re-exports

type Definition = Common.Definition Resolved
type FunctionInfo = Common.FunctionInfo Resolved
