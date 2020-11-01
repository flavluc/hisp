module Env where

import qualified Data.Map as Map

import Expr (Expr(..))
import Err (Err(..))

data Env = Env {
  table :: Map.Map String Expr,
  outer :: Maybe Env
} deriving (Show)

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
  ],
  outer = Nothing
}

insert :: String -> Expr -> Env -> Env
insert k v env = Env {table = Map.insert k v (table env), outer=outer env}

lookup' :: String -> Env -> Maybe Expr
lookup' k env = case Map.lookup k (table env) of
  Just val -> Just val
  Nothing -> outer env >>= lookup' k

fromList :: Env -> [String] -> [Expr] -> Env
fromList env ks es = Env {table=Map.fromList (zip ks es), outer=Just env}

sumArgs :: [Expr] -> Either Err Expr
sumArgs args = Expr.Number . sum <$> evalListOfFloats args

subArgs :: [Expr] -> Either Err Expr
subArgs [] = Left Err {reason = "expected at least one number"}
subArgs (first:args) = (\(pFirst:pArgs) -> Expr.Number (pFirst - sum pArgs)) <$> evalListOfFloats (first:args)

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
  (first:rest) -> Expr.Bool . isSortedBy fn <$> evalListOfFloats (first:rest)
  [] -> Left Err {reason = "expected at least one number"}