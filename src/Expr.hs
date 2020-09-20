module Expr where
  
data Expr = Symbol String
          | Number Float
          | List [Expr]
          deriving (Show, Eq, Ord)