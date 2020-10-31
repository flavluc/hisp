module Repl where

import System.IO (hFlush, stdout)

import Err (Err)
import Env (Env)
import Expr (Expr)
import Eval (evalStr)
import Parser (parse)
import Lexer (tokenize)

repl :: Env -> IO ()
repl env = do
  putStrLn "hisp > "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> return ()
    _ -> case evalStr env input of
      (Left err) -> print err >> repl env
      (Right (env, expr)) -> print expr >> repl env