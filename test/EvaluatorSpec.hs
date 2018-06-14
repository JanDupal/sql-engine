module EvaluatorSpec where

import Test.Hspec
import TestHelper

import Evaluator
import Algebra

spec :: Spec
spec =
  describe "evaluate" $ do
    it "executes simple query" $ do
      evaluate dual `shouldBe` [["dummy", "1"]]

    it "executes simple projection" $ do
      evaluate (Projection ["x"] dual) `shouldBe` [["1"]]

    it "executes repeated projection" $ do
      evaluate (Projection ["x", "dummy", "x", "dummy"] dual) `shouldBe` [["1", "dummy", "1", "dummy"]]

    it "executes empty selection" $ do
      evaluate (Selection (Equal "dummy" "x") dual) `shouldBe` []

    it "executes tautology selection" $ do
      evaluate (Selection (Equal "dummy" "dummy") dual) `shouldBe` [["dummy", "1"]]
