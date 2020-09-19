module Env where

import Data.Map (Map)

import Expr
  
data Env = Env {
  table :: Map String Expr
}