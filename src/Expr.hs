module Expr where

import Data.List (intercalate)

import Err (Err(..))
  
data Expr = Symbol String
          | Number Float
          | Bool Bool
          | List [Expr]
          | Func ([Expr] -> Either Err Expr)

instance Show Expr where
  show (Symbol s) = s
  show (Number n) = show n
  show (Bool b) = show b
  show (List l) = "(" ++ intercalate "," (map show l) ++ ")"
  show (Func f) = "Function {}"

instance Eq Expr where
  (Symbol s1) == (Symbol s2) = s1 == s2
  (Number n1) == (Number n2) = n1 == n2
  (Bool b1) == (Bool b2) = b1 == b2
  (List l1) == (List l2) = l1 == l2
  (Func _) == (Func _) = error "functions can't be compared"