module Expr where

import Err (Err(..))
  
data Expr = Symbol String
          | Number Float
          | List [Expr]
          | Func ([Expr] -> Either Err Expr)