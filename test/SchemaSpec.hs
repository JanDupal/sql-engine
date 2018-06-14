module SchemaSpec where

import Test.Hspec
import TestHelper

import TranslatorM
import Algebra
import Schema

spec :: Spec
spec =
  describe "attributeLookup" $ do
    context "in Relation" $
      it "looks up in a relation" $
        attributeLookup (Relation ["dummy"]) Nothing "dummy" `shouldBe` Right "dummy"

    context "in Alias" $ do
      it "looks up in an alias" $
        attributeLookup dual Nothing "dummy" `shouldBe` Right "dummy"

      it "looks up qualified in an alias" $
        attributeLookup dual (Just "dual") "dummy" `shouldBe` Right "dummy"

    context "in Product" $ do
      it "looks up in a product" $
        attributeLookup (Product (Alias "a" dual) dual) (Just "dual") "dummy" `shouldBe` Right "dummy"

      it "looks up in a nested product" $
        let
          relation = Product
                       (Product (Alias "a" dual) (Alias "b" dual))
                       (Alias "c" dual)
        in attributeLookup relation (Just "c") "dummy" `shouldBe` Right "dummy"

    context "in Projection" $ do
      it "looks up included" $
        attributeLookup (Projection ["x"] dual) Nothing "x" `shouldBe` Right "x"

      it "fails for excluded" $
        attributeLookup (Projection ["x"] dual) Nothing "dummy" `shouldBe` Left (UnknownIdentifier Nothing "dummy")


    context "in Rename" $ do
      it "looks up aliases" $
        attributeLookup (Rename [("x", "y")] dual) Nothing "y" `shouldBe` Right "y"

      it "fails on original names" $
        attributeLookup (Rename [("x", "y")] dual) Nothing "x" `shouldBe` Left (UnknownIdentifier Nothing "x")
