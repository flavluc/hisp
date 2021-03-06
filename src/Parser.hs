module Parser where

import Text.Read (readMaybe)

import Err (Err(..))
import Expr (Expr(..))

parse :: [String] -> Either Err (Expr, [String])
parse [] = Left Err { reason = "could not get token" }
parse (token:rest) = case token of
  "(" -> readSeq rest []
  ")" -> Left Err { reason = "unexprected ')'" }
  _ -> Right (parseAtom token, rest)

readSeq :: [String] -> [Expr] -> Either Err (Expr, [String])
readSeq [] _ = Left Err { reason = "could not find closing ')'"}
readSeq (token:rest) exprs = case token of
  ")" -> Right (Expr.List exprs, rest) -- skip `)`, head to the token after
  _   -> parse (token:rest) >>= \(expr, tokens) -> readSeq tokens (exprs ++ [expr])

parseAtom :: String -> Expr
parseAtom "true" = Expr.Bool True
parseAtom "false" = Expr.Bool False
parseAtom atom = case readMaybe atom of
  Just num -> Expr.Number num
  Nothing -> Expr.Symbol atom