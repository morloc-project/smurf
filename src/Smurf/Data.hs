module Smurf.Data (
    Top(..)
  , Import(..)
  , Statement(..)
  , Source(..)
  , Expression(..)
  , Primitive(..)
  , BExpr(..)
  , AExpr(..)
  , Bop
  , Rop
  , Aop
  , InputType
  , OutputType
  , Type
  , Constraint
  , Name
) where

import Data.List (intersperse)
import Data.Maybe (maybe)

data Top
  = TopImport Import 
  | TopStatement Statement
  | TopSource Source
  deriving(Show, Ord, Eq)

data Source = Source Name [String] deriving(Ord, Eq)

data Statement
  = Signature Name [InputType] OutputType [Constraint]
  | Declaration Name Expression
  deriving(Show, Ord, Eq)

data Expression
  = ExprPrimitive Primitive
  | ExprApplication Name [Expression]
  deriving(Show, Ord, Eq)

data Primitive
  = PrimitiveInt    Integer
  | PrimitiveReal   Double
  | PrimitiveBool   Bool
  | PrimitiveString String
  deriving(Show, Ord, Eq)

data BExpr
  = BExprBool Bool
  | BExprNot BExpr
  | BExprBBinOp Bop BExpr BExpr
  | BExprRBinOp Rop AExpr AExpr
  deriving(Show, Ord, Eq)

data AExpr
  = AExprName    Name
  | AExprInt     Integer
  | AExprReal    Double
  | AExprFunc    Name [AExpr] -- includes array access
  | AExprBinOp   Aop AExpr AExpr
  | AExprNegate  AExpr
  deriving(Show, Ord, Eq)

type Bop = String
type Rop = String
type Aop = String

data Import = Import {
      importPath :: [Name]
    , importQualifier :: Maybe Name
    , importRestriction :: Maybe [Name]
  } deriving(Show, Ord, Eq)

instance Show Source where
  show (Source n ls) = unlines (("source " ++ n) : ls)

type InputType  = Type
type OutputType = Type
type Type       = String
type Constraint = BExpr
type Name       = String
type Op         = String
