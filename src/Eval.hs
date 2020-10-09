module Eval where

import qualified Data.Map as Map

import Expr
import Env
import Err

eval :: Env -> Expr -> Either Err Expr
eval env (Expr.Symbol s) = case Map.lookup s (table env) of
  Just exp -> Right exp
  Nothing -> Left Err {reason = "unexpected symbol k={" ++ s ++ "}"}

eval env (Expr.Number n) = Right (Expr.Number n)

eval env (Expr.List []) = Left Err {reason = "expected a non-empty list"}
eval env (Expr.List (fn:args)) = do
  eFn <- eval env fn
  eArgs <- sequence . map (eval env) $ args
  apply eFn eArgs

eval env (Expr.Func f) = Left Err {reason = "unexpected form"}


apply :: Expr -> [Expr] -> Either Err Expr
apply (Expr.Func f) args = f args
apply _ _ = Left Err {reason = "first form must be a function"}
