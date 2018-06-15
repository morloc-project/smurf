module Smurf.Data (
    Top(..)
  , Import(..)
  , Statement(..)
  , Source(..)
  , Expression(..)
  , Primitive(..)
  , BExpr(..)
  , AExpr(..)
  , MType(..)
  , Name
) where

import Data.List (intersperse)
import Data.Maybe (maybe)

type Name = String
type Tag = String

data Top
  = TopImport Import 
  | TopStatement Statement
  | TopSource Source
  deriving(Show, Ord, Eq)

data Source = Source Name [String] deriving(Ord, Eq)

data MType
  = MSpecific Name [MType] Tag
  | MGeneric Name [MType] Tag
  | MList MType Tag
  | MTuple [MType] Tag 
  | MRecord Name [(Name, MType)] Tag
  | MEmpty
  deriving(Show, Ord, Eq)

data Statement
  = Signature Name [MType] (Maybe MType) [BExpr]
  | Declaration Name Expression
  deriving(Show, Ord, Eq)

data Expression
  = ExprPrimitive Primitive
  -- add tag
  | ExprApplication Name Tag [Expression]
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
  -- relative operators
  | EQ' AExpr AExpr
  | NE' AExpr AExpr
  | GT' AExpr AExpr
  | LT' AExpr AExpr
  | GE' AExpr AExpr
  | LE' AExpr AExpr
  -- logical operators
  | NOT BExpr
  | AND BExpr BExpr
  | OR  BExpr BExpr
  deriving(Show, Ord, Eq)

data AExpr
  = AExprName Name
  | AExprInt Integer
  | AExprReal Double
  | Pos AExpr
  | Neg AExpr
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  | Pow AExpr AExpr
  | Mod AExpr AExpr
  | Quo AExpr AExpr
  deriving(Show, Ord, Eq)

data Import = Import {
      importPath :: [Name]
    , importQualifier :: Maybe Name
    , importRestriction :: Maybe [Name]
  } deriving(Show, Ord, Eq)

instance Show Source where
  show (Source n ls) = unlines (("source " ++ n) : ls)
