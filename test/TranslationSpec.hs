module TranslationSpec where

import Test.Hspec
import TestHelper

import TranslatorM
import qualified Translation as T
import Algebra

translate :: String -> Either TranslationError Relation
translate = T.translate schema . parse

spec :: Spec
spec =
  describe "translate" $ do
    describe "FROM" $ do
      context "with valid" $ do
        it "builds simple select" $
          translate "SELECT * FROM dual" `shouldBe` Right dual

        it "builds aliased select" $
          translate "SELECT * FROM dual d" `shouldBe` Right (Alias "d" dual)

        it "builds cartesian product" $
          translate "SELECT * FROM dual, dual, dual" `shouldBe` Right (Product (Product dual dual) dual)

        it "builds inner join" $
          translate "SELECT * FROM dual a INNER JOIN dual b ON a.dummy = b.dummy" `shouldBe` Right (EtaJoin (Alias "a" dual) (Alias "b" dual) (Equal "dummy" "dummy"))

        it "builds a subquery" $
          translate "SELECT * FROM (SELECT * FROM dual) d" `shouldBe` Right (Alias "d" dual)

      context "with error" $ do
        it "fails on unknown table" $
          translate "SELECT * FROM nothing" `shouldBe` Left (UnknownTable "nothing")

    describe "SELECT" $ do
      context "with valid" $ do
        it "builds projection" $
          translate "SELECT dummy, x FROM dual" `shouldBe` Right (Projection ["dummy", "x"] dual)

        it "builds aliased projection" $
          translate "SELECT a.dummy FROM dual a" `shouldBe` Right (Projection ["dummy"] (Alias "a" dual))

        it "builds aliased projection from multiple sources" $
          shouldBe
            (translate "SELECT a.dummy, b.dummy, c.dummy FROM dual a, dual b, dual c")
            (Right
              (Projection
                ["dummy", "dummy", "dummy"]
                (Product
                  (Product (Alias "a" dual) (Alias "b" dual))
                  (Alias "c" dual))))

        it "builds renaming" $
          translate "SELECT dummy y FROM dual" `shouldBe` Right (Rename [("dummy", "y")] (Projection ["dummy"] dual))

        it "builds renaming with full qualification" $
          translate "SELECT dual.dummy y FROM dual" `shouldBe` Right (Rename [("dummy", "y")] (Projection ["dummy"] dual))

        it "builds selection from subselect" $
          translate "SELECT y FROM (SELECT dual.x y FROM dual)" `shouldBe` Right (Projection ["y"] (Rename [("x", "y")] (Projection ["x"] dual)))

        it "builds renaming from subselect" $
          translate "SELECT d.y z FROM (SELECT dual.x y FROM dual) d" `shouldBe` Right (Rename [("y", "z")] (Projection ["y"] (Alias "d" (Rename [("x", "y")] (Projection ["x"] dual)))))

      context "with error" $ do
        it "fails on unknown column" $
          translate "SELECT unknown FROM dual" `shouldBe` Left (UnknownIdentifier Nothing "unknown")

        it "fails on unknown alias" $
          translate "SELECT unknown.dummy FROM dual" `shouldBe` Left (UnknownIdentifier (Just "unknown") "dummy")

        it "fails on ambiguous column" $
          translate "SELECT dummy FROM dual, dual" `shouldBe` Left (AmbiguousIdentifier Nothing "dummy")

    describe "WHERE" $ do
      context "with valid" $ do
        it "builds simple selection" $
          translate "SELECT * FROM dual WHERE dummy = x" `shouldBe` Right (Selection (Equal "dummy" "x") dual)

      context "with error" $ do
        it "fails on unknown column" $
          translate "SELECT * FROM dual WHERE dummy = unknown" `shouldBe` Left (UnknownIdentifier Nothing "unknown")
