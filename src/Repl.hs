module Repl where

import System.IO (hFlush, stdout)

import Err (Err)
import Env (Env)
import Expr (Expr)
import Eval (evalStr)
import Parser (parse)
import Lexer (tokenize)

printEither :: (Show a, Show b) => Either a b -> IO ()
printEither (Left a) = print a
printEither (Right b) = print b

repl :: Env -> IO ()
repl env = do
  putStrLn "hisp > "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> return ()
    _ -> printEither (evalStr input env) >> repl env