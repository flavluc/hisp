module Err (
  Err(..)
  ) where
  
newtype Err = Err { reason::String } deriving (Eq)

instance Show Err where
  show e = "Error: " ++ reason e