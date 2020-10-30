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
    ("-", Expr.Func subArgs),
    ("=", Expr.Func $ ensureTonicity (==)),
    ("<", Expr.Func $ ensureTonicity (<)),
    (">", Expr.Func $ ensureTonicity (>)),
    ("<=", Expr.Func $ ensureTonicity (<=)),
    (">=", Expr.Func $ ensureTonicity (>=))
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

isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy lte = loop
  where
    loop [] = True
    loop [_] = True
    loop (x:y:zs) = (x `lte` y) && loop (y:zs)

ensureTonicity :: (Float -> Float -> Bool) -> ([Expr] -> Either Err Expr)
ensureTonicity fn = \args -> case args of
  (first:rest) -> evalListOfFloats (first:rest) >>= \eArgs -> Right (Expr.Bool (isSortedBy fn eArgs))
  [] -> Left Err {reason = "expected at least one number"}