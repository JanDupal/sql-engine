module Lib where

import Language.SQL.SimpleSQL.Parser
import qualified Language.SQL.SimpleSQL.Syntax as S

import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L

data ReturnField = ReturnField {
    name :: String
  , dataSource :: DataSource
  , column :: String
} deriving Show

data Query = Query {
    returnFields :: [ReturnField]
} deriving Show

parse :: String -> S.QueryExpr
parse sql =
  case parseQueryExpr S.SQL2011 "sql" Nothing sql of
    Right expr -> expr
    Left err   -> error $ show err

-- Context

data DataSource =
    Table {
        tableName :: String
      , columns :: [String]
    }
  | SubQuery Query
  deriving Show

type Scope = M.Map String DataSource

data Context = Context {
     schema :: [DataSource]
  ,  scope :: Scope
}

findTable :: Context -> String -> DataSource
findTable ctx identifier =
  case L.find (\t -> identifier == tableName t) (schema ctx) of
    Just table -> table
    Nothing    -> error $ "Unknown reference " ++ identifier

findColumn :: Context -> String -> String -> Maybe (DataSource, String)
findColumn Context{ scope = scope } table column =
  case M.lookup table scope of
    Just t@Table{ columns = columns } -> if column `elem` columns then Just (t, column) else Nothing
    Just subQuery@SubQuery{}              -> error "Subqueries are not supported yet."
    Nothing                               -> Nothing


-- Data Sources

collectDataSources :: Context -> S.QueryExpr -> Context
collectDataSources ctx@Context{ scope = prevScope } S.Select{ S.qeFrom = from } =
  ctx { scope = M.union scope prevScope }
  where
    scope = M.fromList $ map (tableRefToDataSource ctx) from

tableRefToDataSource :: Context -> S.TableRef -> (String, DataSource)
tableRefToDataSource ctx (S.TRSimple [S.Name name]) = (name, findTable ctx name)
tableRefToDataSource ctx (S.TRAlias tRef (S.Alias (S.Name alias) Nothing)) = (alias, snd $ tableRefToDataSource ctx tRef)
tableRefToDataSource ctx other = error $ "Unsupported TableRef: " ++ show other

-- Return Fields

collectReturnFields :: Context -> S.QueryExpr -> [ReturnField]
collectReturnFields ctx S.Select{ S.qeSelectList = list } = map (selectItemToReturnField ctx) list

selectItemToReturnField :: Context -> (S.ValueExpr, Maybe S.Name) -> ReturnField
selectItemToReturnField ctx (S.Iden [S.Name table, S.Name column], Just (S.Name alias)) =
  case findColumn ctx table column of
    Just (ds, column) -> ReturnField { name = alias, dataSource = ds, column = column }
    Nothing         -> error $ "Unknown reference " ++ alias
selectItemToReturnField _ selectItem = error $ "Could not convert " ++ show selectItem

-- Build

build :: S.QueryExpr -> Query
build sql =
  let baseCtx = Context { schema = [Table{ tableName = "dual", columns = ["dummy"] }], scope = M.empty }
      ctx = collectDataSources baseCtx sql
      fields = collectReturnFields ctx sql
  in Query { returnFields = fields }

someFunc :: IO ()
someFunc = print $ build $ parse "SELECT y.dummy x FROM dual y"
