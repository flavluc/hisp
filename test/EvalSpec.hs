module EvalSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Eval
import Env
import Expr
import Err

eval' :: String -> Either Err Expr
eval' = fmap snd . evalStr defaultEnv

spec :: Spec
spec = do
  describe "eval" $ do
    it "should evaluate different expressions" $ do
      eval' "0" `shouldBe` Right (Expr.Number 0)
      eval' "(< 0 1)" `shouldBe` Right (Expr.Bool True)
      eval' "(<= 0 1)" `shouldBe` Right (Expr.Bool True)
      eval' "(> 1 0)" `shouldBe` Right (Expr.Bool True)
      eval' "(>= 1 0)" `shouldBe` Right (Expr.Bool True)
      eval' "(= 0 0 0 0)" `shouldBe` Right (Expr.Bool True)
      eval' "(= 0 0 1 0)" `shouldBe` Right (Expr.Bool False)
      eval' "(if (< 0 1) 1 2)" `shouldBe` Right (Expr.Number 1)
      eval' "(if (> 0 1) 1 2)" `shouldBe` Right (Expr.Number 2)
      (\(env, _) -> lookup' "a" env) <$> evalStr defaultEnv "(def a 1)" `shouldBe` Right(Just(Expr.Number 1))