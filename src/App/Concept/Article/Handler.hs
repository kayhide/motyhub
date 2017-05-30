{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Concept.Article.Handler
  ( handlers
  ) where

import Data.Maybe
import Control.Monad
import Control.Lens
import Database.Persist
import Servant

import App.Model
import App.Monad.Handleable
import qualified App.Concept.Article.Operation as Article
import qualified App.Concept.Blog.Operation as Blog
import App.Concept.Article.Serializer


handlers blogId = index' blogId :<|> show' blogId :<|> create' blogId :<|> update' blogId :<|> destroy' blogId


index' :: BlogId -> Handleable [Entity Article]
index' blogId = do
  blog' <- operate $ Blog.lookup blogId
  verifyPresence blog'
  operate $ Article.allOf blogId

show' :: BlogId -> ArticleId -> Handleable (Entity Article)
show' blogId articleId = do
  article' <- operate $ Article.lookupOf blogId articleId
  verifyPresence article'

create' :: BlogId -> ArticleForCreate -> Handleable (Entity Article)
create' blogId articleForCreate = do
  operate $ Article.create changeset
  where
    changeset = (ArticleBlogId =. blogId) : toChangeset articleForCreate

update' :: BlogId -> ArticleId -> ArticleForUpdate -> Handleable (Entity Article)
update' blogId articleId articleForUpdate = do
  article <- show' blogId articleId
  operate $ Article.update article changeset
  where
    changeset = toChangeset articleForUpdate

destroy' :: BlogId -> ArticleId -> Handleable NoContent
destroy' blogId articleId = do
  article <- show' blogId articleId
  operate $ Article.destroy article
  return NoContent
