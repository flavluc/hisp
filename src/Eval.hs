module Eval where

import qualified Data.Map as Map

import Expr
import Env
import Err
import Parser
import Lexer

evalStr :: String -> Env -> Either Err Expr
evalStr str env = do
  (expr, _) <- parse (tokenize str)
  eExpr <- eval env expr
  Right eExpr

eval :: Env -> Expr -> Either Err Expr
eval env (Expr.Symbol s) = case Map.lookup s (table env) of
  Just exp -> Right exp
  Nothing -> Left Err {reason = "unexpected symbol k={" ++ s ++ "}"}

eval env (Expr.Number n) = Right (Expr.Number n)

eval env (Expr.Bool b) = Right (Expr.Bool b)

eval env (Expr.List []) = Left Err {reason = "expected a non-empty list"}
eval env (Expr.List (op:args)) = case op of
  Expr.Symbol "if" -> evalIf args env
  _ -> evalFn op args env
      
eval env (Expr.Func f) = Left Err {reason = "unexpected form"}


evalFn :: Expr -> [Expr] -> Env -> Either Err Expr
evalFn op args env = do
  eOp <- eval env op
  eArgs <- sequence . map (eval env) $ args
  apply eOp eArgs
  where apply (Expr.Func f) args = f args
        apply _ _ = Left Err {reason = "first form must be a function"}

evalIf :: [Expr] -> Env -> Either Err Expr
evalIf [c,t,e] env = eval env c >>= chooseExpr
  where chooseExpr (Expr.Bool cond) = eval env (if cond then t else e)
        chooseExpr _ = Left Err {reason="expected condition to be a Bool expression"}
evalIf _ _ = Left Err {reason="invalid conditional expression"}