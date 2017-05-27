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
import qualified App.Concept.Blog.Operation as Blog
import App.Concept.Blog.Serializer


handlers = index' :<|> show' :<|> create' :<|> update' :<|> destroy'

verifyPresence :: Maybe a -> Handleable a
verifyPresence (Just x) = return x
verifyPresence Nothing  = throwError err404

index' :: Handleable [Entity Blog]
index' = operate Blog.all

show' :: BlogId -> Handleable (Entity Blog)
show' blogId = do
  blog' <- operate $ Blog.lookup blogId
  verifyPresence blog'

create' :: BlogForCreate -> Handleable (Entity Blog)
create' blogForCreate
  = operate $ Blog.create $ toChangeset blogForCreate

update' :: BlogId -> BlogForUpdate -> Handleable (Entity Blog)
update' blogId blogForUpdate = do
  blog' <- operate $ do
    blog' <- Blog.lookup blogId
    mapM (flip Blog.update changeset) blog'
  verifyPresence blog'
  where
    changeset = toChangeset blogForUpdate

destroy' :: BlogId -> Handleable NoContent
destroy' blogId = do
  blog' <- operate $ do
    blog' <- Blog.lookup blogId
    mapM Blog.destroy blog'
  verifyPresence blog'
  return NoContent
