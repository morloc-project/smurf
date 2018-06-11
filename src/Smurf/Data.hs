module Smurf.Data (
    Smurf(..)
  , Statement(..)
  , InputType
  , OutputType
  , Type
  , Constraint
  , Name
) where

import Data.List (intersperse)

data Smurf = Smurf [Statement]
data Statement
  = Signature Name [InputType] OutputType [Constraint]
  | Source Name [String]
  deriving(Ord, Eq)

showStatement' :: Name -> [InputType] -> OutputType -> String
showStatement' n ins o
  = n
  ++ " :: "
  ++ (concat . intersperse ", " . map show) ins
  ++ " -> "
  ++ o

instance Show Statement where
  show (Signature n ins o []) =
    showStatement' n ins o 
  show (Signature n ins o cs) =
    (showStatement' n ins o) ++ " where\n" ++ 
    (unlines . map ((++) "  ") . (map show) $ cs) 
  show (Source n ls) =
    unlines (("source " ++ n) : ls)

instance Show Smurf where
  show (Smurf xs) = unlines . map show $ xs

type InputType  = Type
type OutputType = Type
type Type       = String
type Constraint = String
type Name       = String
