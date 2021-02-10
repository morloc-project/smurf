module Smurf.Data (
    Top(..)
  , Statement(..)
  , Expression(..)
  , Primitive(..)
  , MType(..)
  , Name
) where

import Data.List (intersperse)
import Data.Maybe (maybe)

type Name = String
type Tag = String

newtype Top
  = TopStatement Statement
  deriving(Show, Ord, Eq)

data MType
  = MSpecific Name [MType] Tag
  | MGeneric Name [MType] Tag
  | MList MType Tag
  | MTuple [MType] Tag 
  | MRecord Name [(Name, MType)] Tag
  | MEmpty
  deriving(Show, Ord, Eq)

data Statement
  = Signature
      Name           -- lhs
      [MType]        -- inputs
      (Maybe MType)  -- optional output
  | Declaration
      Name           -- lhs
      [Name]         -- bound variables
      Expression     -- rhs
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

