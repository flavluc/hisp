module EvalSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Eval
import Env
import Expr

spec :: Spec
spec = do
  describe "eval" $ do
    it "should evaluate different expressions" $ do
      evalStr "0" defaultEnv `shouldBe` Right (Expr.Number 0)
      evalStr "(< 0 1)" defaultEnv `shouldBe` Right (Expr.Bool True)
      evalStr "(<= 0 1)" defaultEnv `shouldBe` Right (Expr.Bool True)
      evalStr "(> 1 0)" defaultEnv `shouldBe` Right (Expr.Bool True)
      evalStr "(>= 1 0)" defaultEnv `shouldBe` Right (Expr.Bool True)
      evalStr "(= 0 0 0 0)" defaultEnv `shouldBe` Right (Expr.Bool True)
      evalStr "(= 0 0 1 0)" defaultEnv `shouldBe` Right (Expr.Bool False)