module Lexer where

  replace :: Char -> String -> String -> String
  replace pat sub = foldr (\c s -> if c == pat then sub ++ s else c:s) ""

  tokenize :: String -> [String]
  tokenize = words . replace '(' " ( " . replace ')' " ) "

