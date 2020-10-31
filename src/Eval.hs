module Eval where

import Expr
import Env
import Err
import Parser
import Lexer

evalStr :: Env -> String -> Either Err (Env, Expr)
evalStr env str = do
  (expr, _) <- parse (tokenize str)
  (env, expr) <- eval env expr
  Right (env, expr)

eval :: Env -> Expr -> Either Err (Env, Expr)
eval env (Expr.Symbol s) = case lookup' s env of
  Just exp -> Right (env, exp)
  Nothing -> Left Err {reason = "unexpected symbol k={" ++ s ++ "}"}

eval env (Expr.Number n) = Right (env, Expr.Number n)

eval env (Expr.Bool b) = Right (env, Expr.Bool b)

eval env (Expr.List []) = Left Err {reason = "expected a non-empty list"}
eval env (Expr.List (op:args)) = case op of
  Expr.Symbol "if" -> evalIf args env
  Expr.Symbol "def" -> evalDef args env
  _ -> evalOp op args env
      
eval env (Expr.Func f) = Left Err {reason = "unexpected form"}


evalOp :: Expr -> [Expr] -> Env -> Either Err (Env, Expr)
evalOp op args env = do
  (_, op) <- eval env op
  args <- sequence . map (eval env) $ args
  apply op (map snd args)
  where apply (Expr.Func f) args = f args >>= Right . (,) env
        apply _ _ = Left Err {reason = "first form must be a function"}

evalIf :: [Expr] -> Env -> Either Err (Env, Expr)
evalIf [c,t,e] env = eval env c >>= \(_, cond) -> chooseExpr cond
  where chooseExpr (Expr.Bool cond) = eval env (if cond then t else e)
        chooseExpr _ = Left Err {reason="expected condition to be a Bool expression"}
evalIf _ _ = Left Err {reason="invalid conditional expression"}

evalDef :: [Expr] -> Env -> Either Err (Env, Expr)
evalDef [Expr.Symbol name, expr] env = eval env expr >>= \(_, expr) ->
  Right (insert name expr env, Expr.Symbol name)
evalDef _ _ = Left Err {reason="invalid definition expression"}
