module Smurf.Data (
    Top(..)
  , Import(..)
  , Statement(..)
  , Source(..)
  , Expression(..)
  , Primitive(..)
  , BExpr(..)
  , AExpr(..)
  , UnaryOp(..)
  , BinOp(..)
  , Bop
  , Rop
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
  = Signature Name [InputType] (Maybe OutputType) [Constraint]
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
  = BExprName Name
  | BExprBool Bool
  | BExprNot BExpr
  | BExprBBinOp Bop BExpr BExpr
  | BExprRBinOp Rop AExpr AExpr
  deriving(Show, Ord, Eq)

data AExpr
  = AExprName Name
  | AExprInt Integer
  | AExprReal Double
  | AExprBinOp BinOp AExpr AExpr
  | AExprUnaryOp UnaryOp AExpr
  | AExprUnOp AExpr
  deriving(Show, Ord, Eq)

data UnaryOp = Neg | Pos
  deriving(Show, Ord, Eq)

data BinOp = Add | Sub | Mul | Div | Pow | Mod | Quo
  deriving(Show, Ord, Eq)

type Bop = String
type Rop = String

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
