{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Concept.Article.Handler where

import Data.Maybe
import Control.Monad
import Database.Persist
import Servant

import App.Model
import App.Monad.Handleable
import qualified App.Concept.Article.Operation as Article
import App.Concept.Article.Serializer

handlers blogId = index' blogId :<|> show' blogId :<|> create' blogId :<|> update' blogId :<|> destroy' blogId

index' :: BlogId -> Handleable [Entity Article]
index' blogId = operate Article.all


verifyPresence :: Maybe a -> Handleable a
verifyPresence (Just x) = return x
verifyPresence Nothing  = throwError err404

show' :: BlogId -> ArticleId -> Handleable (Entity Article)
show' blogId key = do
  article' <- operate $ Article.lookup key
  verifyPresence article'

create' :: BlogId -> ArticleForCreate -> Handleable (Entity Article)
create' blogId (ArticleForCreate title body basename)
  = operate $ Article.create updates
  where
    updates = (ArticleBlogId =. blogId) : catMaybes
      [ (ArticleTitle =.) <$> title
      , (ArticleBody =.) <$> body
      , (ArticleBasename =.) <$> basename
      ]

update' :: BlogId -> ArticleId -> ArticleForUpdate -> Handleable (Entity Article)
update' _ key (ArticleForUpdate blogId title body basename)
  = do
  article' <- operate $ do
    article' <- Article.lookup key
    mapM (Article.update updates) article'
  verifyPresence article'
  where
    updates = catMaybes
      [ (ArticleBlogId =.) <$> blogId
      , (ArticleTitle =.) <$> title
      , (ArticleBody =.) <$> body
      , (ArticleBasename =.) <$> basename
      ]

destroy' :: BlogId -> ArticleId -> Handleable NoContent
destroy' blogId key = do
  article' <- operate $ do
    article' <- Article.lookup key
    mapM Article.destroy article'
  case article' of
    Just _  -> return NoContent
    Nothing -> throwError err404
