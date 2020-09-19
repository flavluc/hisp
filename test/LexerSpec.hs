module LexerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Lexer

spec :: Spec
spec = do
  describe "replace" $ do
    it "should replaces strings correctly" $ do
      replace 'a' "aa" "baba" `shouldBe` "baabaa"

  describe "tokenize" $ do
      it "should tokenize correctly" $ do
        tokenize "(+ 1 2)" `shouldBe` ["(", "+", "1", "2", ")"]

