module Env where

import qualified Data.Map as Map

import Expr (Expr(..))
import Err (Err(..))
  
data Env = Env {
  table :: Map.Map String Expr
}

defaultEnv :: Env
defaultEnv = Env {
  table = Map.fromList [
    ("+", Expr.Func sumArgs),
    ("-", Expr.Func subArgs)
  ]
}

sumArgs :: [Expr] -> Either Err Expr
sumArgs args = evalListOfFloats args >>= \pArgs -> Right (Expr.Number (sum pArgs))

subArgs :: [Expr] -> Either Err Expr
subArgs [] = Left Err {reason = "expected at least one number"}
subArgs (first:args) = evalListOfFloats (first:args) >>= \(pFirst:pArgs) -> Right (Expr.Number (pFirst - sum pArgs))

evalListOfFloats :: [Expr] -> Either Err [Float]
evalListOfFloats = sequence . (map evalSingleFloat)

evalSingleFloat :: Expr -> Either Err Float
evalSingleFloat (Expr.Number num) = Right num
evalSingleFloat _ = Left Err {reason = "expected a number"}