{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Concept.Article.Handler
  ( handlers
  ) where

import Data.Maybe
import Control.Monad
import Database.Persist
import Servant

import App.Model
import App.Monad.Handleable
import qualified App.Concept.Article.Operation as Article
import App.Concept.Article.Serializer


handlers blogId = index' blogId :<|> show' blogId :<|> create' blogId :<|> update' blogId :<|> destroy' blogId

verifyPresence :: Maybe a -> Handleable a
verifyPresence (Just x) = return x
verifyPresence Nothing  = throwError err404

index' :: BlogId -> Handleable [Entity Article]
index' blogId = operate $ Article.allOf blogId

show' :: BlogId -> ArticleId -> Handleable (Entity Article)
show' blogId articleId = do
  article' <- operate $ Article.lookup articleId
  verifyPresence article'

create' :: BlogId -> ArticleForCreate -> Handleable (Entity Article)
create' blogId articleForCreate
  = operate $ Article.create changeset
  where
    changeset = (ArticleBlogId =. blogId) : toChangeset articleForCreate

update' :: BlogId -> ArticleId -> ArticleForUpdate -> Handleable (Entity Article)
update' _ articleId articleForUpdate = do
  article' <- operate $ do
    article' <- Article.lookup articleId
    mapM (flip Article.update changeset) article'
  verifyPresence article'
  where
    changeset = toChangeset articleForUpdate

destroy' :: BlogId -> ArticleId -> Handleable NoContent
destroy' blogId articleId = do
  article' <- operate $ do
    article' <- Article.lookup articleId
    mapM Article.destroy article'
  verifyPresence article'
  return NoContent
