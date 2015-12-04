module TestHelper where

import Language.SQL.SimpleSQL.Parser
import qualified Language.SQL.SimpleSQL.Syntax as S

import Debug.Trace

import Schema
import Algebra

parse :: String -> S.QueryExpr
parse sql =
  case parseQueryExpr S.SQL2011 "test" Nothing sql of
    Right expr -> expr
    Left err   -> error $ show err

dual :: Relation
dual = Alias "dual" $ Relation ["dummy", "x"]

schema :: Schema
schema = Schema [dual]
