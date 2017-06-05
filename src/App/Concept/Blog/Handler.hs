{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Concept.Blog.Handler
  ( handlers
  ) where

import Data.Maybe
import Control.Monad
import Database.Persist
import Servant

import App.Model
import App.Monad.Handleable
import Mid.Db.Monad
import qualified App.Concept.Blog.Operation as Blog
import App.Concept.Blog.Serializer


handlers = index' :<|> show' :<|> create' :<|> update' :<|> destroy'


index' :: Handleable [Entity Blog]
index' = runDb Blog.all

show' :: BlogId -> Handleable (Entity Blog)
show' blogId = do
  blog' <- runDb $ Blog.lookup blogId
  verifyPresence blog'

create' :: BlogForCreate -> Handleable (Entity Blog)
create' blogForCreate = runDb $ Blog.create changeset
  where
    changeset = toChangeset blogForCreate

update' :: BlogId -> BlogForUpdate -> Handleable (Entity Blog)
update' blogId blogForUpdate = do
  blog <- show' blogId
  runDb $ Blog.update blog changeset
  where
    changeset = toChangeset blogForUpdate

destroy' :: BlogId -> Handleable NoContent
destroy' blogId = do
  blog <- show' blogId
  runDb $ Blog.destroy blog
  return NoContent
