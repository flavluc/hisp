module ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Parser
import Lexer
import Expr

runParser :: String -> Expr
runParser str = case parse (tokenize str) of
  Right (expr, _) -> expr
  Left err -> error (show err)

spec :: Spec
spec = do
  describe "parseAtom" $ do
    it "should parse different atoms" $ do
      parseAtom "0" `shouldBe` Expr.Number 0
      parseAtom "+" `shouldBe` Expr.Symbol "+"

  describe "parse" $ do
    it "should parse different expressions" $ do
       runParser "(+ 0 1)" `shouldBe` Expr.List [Expr.Symbol "+", Expr.Number 0, Expr.Number 1]