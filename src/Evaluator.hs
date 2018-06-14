module Evaluator where

import Algebra

import Data.Maybe
import Data.List

type Cell = String
type Row = [Cell]
type Table = [Row]

evaluate :: Relation -> Table
evaluate (Alias alias (Relation _)) = readTable alias

evaluate (Projection attrs rel) = map projector (evaluate rel)
  where
    relationAttrs = availableAttributes rel
    lookup :: [Int]
    lookup = map fromJust $ map (\a -> elemIndex a relationAttrs) attrs
    projector :: Row -> Row
    projector row = map (row !!) lookup

evaluate (Selection (Equal a b) rel) = filter selector (evaluate rel)
  where
    relationAttrs = availableAttributes rel
    projector :: Attribute -> Row -> Cell
    projector attr row = row !! fromJust (elemIndex attr relationAttrs)
    selector :: Row -> Bool
    selector row = (projector a row) == (projector b row)

availableAttributes :: Relation -> [Attribute]
availableAttributes (Relation attrs) = attrs
availableAttributes (Alias _ rel) = availableAttributes rel
availableAttributes (Projection attrs _) = attrs

readTable :: String -> Table
readTable "dual" = [["dummy", "1"]]

