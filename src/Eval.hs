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
  Nothing -> Left Err {reason="unexpected symbol k={" ++ s ++ "}"}

eval env (Expr.Number n) = Right (env, Expr.Number n)

eval env (Expr.Bool b) = Right (env, Expr.Bool b)

eval env (Expr.List []) = Left Err {reason="expected a non-empty list"}
eval env (Expr.List (op:args)) = case op of
  Expr.Symbol "if" -> evalIf args env
  Expr.Symbol "def" -> evalDef args env
  Expr.Symbol "fn" -> evalFn args env
  _ -> evalOp op args env
      
eval env (Expr.Lambda _) = Left Err {reason="unexpected form"}

eval env (Expr.Func _) = Left Err {reason="unexpected form"}


evalOp :: Expr -> [Expr] -> Env -> Either Err (Env, Expr)
evalOp op args env = eval env op >>= \(_, op) -> case op of
  (Expr.Func f) -> apply f args env
  (Expr.Lambda l) -> envForLambda env (params l) args >>= \env -> eval env (body l)
  _ -> Left Err {reason="first form must be a function"}

evalArgs :: Env -> [Expr] -> Either Err [Expr]
evalArgs env = sequence . map (fmap snd . eval env)

-- @TODO: should I use Arrows?
apply :: ([Expr] -> Either Err Expr) -> [Expr] -> Env -> Either Err (Env, Expr)
apply f args env = (,) env <$> (evalArgs env args >>= f)

envForLambda :: Env -> Expr -> [Expr] -> Either Err Env
envForLambda env formal actual = evalFormalArgs env formal >>= createEnv
  where createEnv formal
                    | length formal == length actual = fromList env formal <$> evalArgs env actual
                    | otherwise = Left Err {reason="expected "++show(length formal)++" arguments, got "++show(length actual)}

evalFormalArgs :: Env -> Expr -> Either Err [String]
evalFormalArgs env (Expr.List l) = sequence . map evalListOfSymbols $ l
  where evalListOfSymbols (Symbol s) = Right s
        evalListOfSymbols _ = Left Err {reason="expected symbols in the argument list"}
evalFormalArgs _ _ = Left Err {reason="expected args to be a list"}

evalIf :: [Expr] -> Env -> Either Err (Env, Expr)
evalIf [c,t,e] env = eval env c >>= \(_, cond) -> chooseExpr cond
  where chooseExpr (Expr.Bool cond) = eval env (if cond then t else e)
        chooseExpr _ = Left Err {reason="expected condition to be a Bool expression"}
evalIf _ _ = Left Err {reason="invalid conditional expression"}

evalDef :: [Expr] -> Env -> Either Err (Env, Expr)
evalDef [Expr.Symbol name, expr] env = eval env expr >>= \(_, expr) ->
  Right (insert name expr env, Expr.Symbol name)
evalDef _ _ = Left Err {reason="invalid definition expression"}

evalFn :: [Expr] -> Env -> Either Err (Env, Expr)
evalFn [params, body] env = Right(env, Expr.Lambda(LambdaExpr {params=params, body=body}))
evalFn _ _ = Left Err {reason="invalid lambda expression"}