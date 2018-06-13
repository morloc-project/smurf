module Smurf.Data (
    Top(..)
  , Import(..)
  , Statement(..)
  , Source(..)
  , Expression(..)
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
  = ExprName Name 
  | ExprPrimitive Primitive
  | ExprApplication Name [Expression]
  deriving(Show, Ord, Eq)

data Primitive
  = PrimitiveInt    Int
  | PrimitiveReal   Double
  | PrimitiveBool   Bool
  | PrimitiveString String
  deriving(Show, Ord, Eq)

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
type Constraint = String
type Name       = String
