module Translation (Translator, translate) where

import Algebra
import Schema

import TranslatorM

import Control.Monad
import qualified Language.SQL.SimpleSQL.Syntax as S
import Data.Maybe

translate :: Schema -> S.QueryExpr -> Translator Relation
translate schema S.Select{ S.qeFrom = from, S.qeWhere = wher, S.qeSelectList = select } = do
  relation'  <- translateFrom schema from
  relation'' <- maybe (return relation') (translateWhere relation') wher
  relation   <- translateSelect relation'' select
  return $ simplify relation

simplify :: Relation -> Relation
simplify (Rename [] relation) = relation
simplify relation = relation

-- From

translateFrom :: Schema -> [S.TableRef] -> Translator Relation
translateFrom schema tableRefs = do
    x:xs <- mapM (tableRefToRelation schema) tableRefs
    foldM joinRelations x xs


tableRefToRelation :: Schema -> S.TableRef -> Translator Relation
tableRefToRelation schema (S.TRSimple [S.Name name]) = schemaLookup schema name
tableRefToRelation schema (S.TRAlias tableRef (S.Alias (S.Name alias) Nothing)) = do
  relation <- tableRefToRelation schema tableRef
  return $ Alias alias relation
tableRefToRelation schema (S.TRJoin tableRefLeft False joinType tableRefRight (Just (S.JoinOn expr))) = do
  left  <- tableRefToRelation schema tableRefLeft
  right <- tableRefToRelation schema tableRefRight
  formula <- translateFormula (Product left right) expr
  return $ EtaJoin left right formula
tableRefToRelation schema (S.TRQueryExpr subquery) = translate schema subquery

joinRelations :: Relation -> Relation -> Translator Relation
joinRelations r s = return $ Product r s

-- Select

translateSelect :: Relation -> [(S.ValueExpr, Maybe S.Name)] -> Translator Relation
translateSelect relation [(S.Star, Nothing)] = return relation
translateSelect relation listItems = do
  attributes <- mapM (selectItemToProjection relation . fst) listItems
  let
    renaming = map (\(a, Just (S.Name n)) -> (a, n)) $ filter (isJust . snd) $ zip attributes (map snd listItems)
  return $ Rename renaming $ Projection attributes relation

selectItemToProjection :: Relation -> S.ValueExpr -> Translator Attribute
selectItemToProjection relation (S.Iden [S.Name name]) = attributeLookup relation Nothing name
selectItemToProjection relation (S.Iden [S.Name source, S.Name name]) = attributeLookup relation (Just source) name

-- Where

translateWhere :: Relation -> S.ValueExpr -> Translator Relation
translateWhere relation expr = do
  formula <- translateFormula relation expr
  return $ Selection formula relation

-- Formula

translateFormula :: Relation -> S.ValueExpr -> Translator Formula
translateFormula relation (S.BinOp (S.Iden a) [S.Name "="] (S.Iden b)) = do
  left <- translateIdentifier relation a
  right <- translateIdentifier relation b
  return $ Equal left right

-- identifier
translateIdentifier :: Relation -> [S.Name] -> Translator Attribute
translateIdentifier relation [S.Name name] = attributeLookup relation Nothing name
translateIdentifier relation [S.Name source, S.Name name] = attributeLookup relation (Just source) name
