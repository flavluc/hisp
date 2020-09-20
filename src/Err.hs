module Err (
  Err(..)
  ) where
  
newtype Err = Err { reason::String }

instance Show Err where
  show e = "Erorr: " ++ reason e