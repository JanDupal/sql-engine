{-# LANGUAGE GADTs #-}
module Algebra where

type Attribute = String

data Relation where
  Relation   :: [Attribute] -> Relation
  Alias      :: String -> Relation -> Relation
  Product    :: Relation -> Relation -> Relation
  Projection :: [Attribute] -> Relation -> Relation
  Selection  :: Formula -> Relation -> Relation
  Rename     :: [(Attribute, Attribute)] -> Relation -> Relation
  EtaJoin    :: Relation -> Relation -> Formula -> Relation
  deriving (Eq, Show)

data Formula where
  Equal :: Attribute -> Attribute -> Formula
  deriving (Eq, Show)
