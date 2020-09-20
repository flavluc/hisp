module Main where

main :: IO ()
main = do let a = "ola" in
            case a of 
              "ca"  -> putStrLn "ca"
              "ola" -> putStrLn "ola"
              _     -> putStrLn "_"