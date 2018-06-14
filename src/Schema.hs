module Schema (Schema (..), schemaLookup, attributeLookup) where

import Algebra
import TranslatorM

import Control.Monad.Except

import Data.List

data Schema = Schema [Relation]

schemaLookup :: Schema -> String -> Translator Relation
schemaLookup (Schema relations) name =
  case find (\(Alias r (Relation _))-> r == name) relations of
    Just r  -> return r
    Nothing -> throwError $ UnknownTable name

attributeLookup :: Relation -> Maybe String -> String -> Translator Attribute
attributeLookup relation source identifier = do
  maybeAttr <- attributeLookup_ relation source identifier
  maybe (throwError $ UnknownIdentifier source identifier) return maybeAttr


attributeLookup_ :: Relation -> Maybe String -> String -> Translator (Maybe Attribute)
attributeLookup_ (Relation attrs) Nothing identifier =
  return $ find (== identifier) attrs

attributeLookup_ (Alias _ r) Nothing identifier = attributeLookup_ r Nothing identifier
attributeLookup_ (Alias name r) (Just source) identifier =
  if name == source then
    attributeLookup_ r Nothing identifier
   else
     return Nothing

attributeLookup_ (Product left right) source identifier = do
  leftMaybeAttr  <- attributeLookup_ left source identifier
  rightMaybeAttr <- attributeLookup_ right source identifier
  case (leftMaybeAttr, rightMaybeAttr) of
    (Just attr, Nothing) -> return $ Just attr
    (Nothing, Just attr) -> return $ Just attr
    (Just _, Just _)     -> throwError $ AmbiguousIdentifier source identifier
    (Nothing, Nothing)   -> return Nothing

attributeLookup_ (Projection attrs relation) Nothing identifier =
  if identifier `elem` attrs then
    attributeLookup_ relation Nothing identifier
  else
    return Nothing

attributeLookup_ (Rename mapping relation) Nothing identifier =
  case findByNewAlias of
    Just (_, v) -> return $ Just v
    Nothing     -> case findByOriginalName of
      Just _ -> return Nothing
      Nothing -> attributeLookup_ relation Nothing identifier
  where
    findByNewAlias = find (\(_, v) -> v == identifier) mapping
    findByOriginalName = find (\(k, _) -> k == identifier) mapping