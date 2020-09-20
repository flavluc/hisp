module Lexer where

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace pat sub str =
    if take len str == pat
    then sub ++ replace pat sub (drop len str)
    else head str : replace pat sub (tail str)
  where len = length pat

tokenize :: String -> [String]
tokenize = words . replace "(" " ( " . replace ")" " ) "