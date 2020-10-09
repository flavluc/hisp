module Repl where

import System.IO (hFlush, stdout)

import Err (Err)
import Env (Env)
import Expr (Expr)
import Eval (eval)
import Parser (parse)
import Lexer (tokenize)

printEither :: (Show a, Show b) => Either a b -> IO ()
printEither (Left a) = print a
printEither (Right b) = print b

eval' :: String -> Env -> Either Err Expr
eval' str env = do
  (expr, _) <- parse (tokenize str)
  eExpr <- eval env expr
  Right eExpr

repl :: Env -> IO ()
repl env = do
  putStrLn "hisp > "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> return ()
    _ -> printEither (eval' input env) >> repl env