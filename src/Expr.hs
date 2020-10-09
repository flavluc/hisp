module Expr where

import Data.List (intercalate)

import Err (Err(..))
  
data Expr = Symbol String
          | Number Float
          | List [Expr]
          | Func ([Expr] -> Either Err Expr)



instance Show Expr where
  show (Symbol s) = s
  show (Number n) = show n
  show (List l) = "(" ++ intercalate "," (map show l) ++ ")"
  show (Func f) = "Function {}"