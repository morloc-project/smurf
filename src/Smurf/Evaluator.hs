module Smurf.Evaluator where

import Smurf.AST

data Value
  = VBool Bool
  | VInt Integer
  deriving Show
            
eval :: Term -> Maybe Value
eval STrue = Just (VBool True)
eval SFalse = Just (VBool False)
eval SZero = Just (VInt 0)

eval (SIsZero t) = 
  case eval t of
    Just (VInt i) -> Just (VBool (i == 0))
    _ -> Nothing

eval (SSucc t) =
  case eval t of
    Just (VInt i) -> Just (VInt (i+1))
    _ -> Nothing

eval (SPred t) = 
  case eval t of
    Just (VInt i) | i>0 -> Just(VInt (i-1))
    _ -> Nothing

eval (SIfThen t1 t2 t3) =
  case eval t1 of
    Just (VBool True) -> eval t2
    Just (VBool False) -> eval t3
    _ -> Nothing
