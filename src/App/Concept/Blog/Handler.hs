{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Concept.Blog.Handler where

import Data.Maybe
import Control.Monad
import Database.Persist
import Servant

import App.Model
import App.Monad.Handleable
import qualified App.Concept.Blog.Operation as Blog
import App.Concept.Blog.Serializer

handlers = index' :<|> show' :<|> create' :<|> update' :<|> destroy'

index' :: Handleable [Entity Blog]
index' = operate Blog.all

show' :: BlogId -> Handleable (Entity Blog)
show' key = do
  blog' <- operate $ Blog.lookup key
  case blog' of
    Just blog -> return blog
    Nothing   -> throwError err404

create' :: BlogForCreate -> Handleable (Entity Blog)
create' (BlogForCreate hostUrl username password url title)
  = operate $ Blog.create updates
  where
    updates = catMaybes
      [ (BlogHostUrl =.) <$> hostUrl
      , (BlogUsername =.) <$> username
      , (BlogPassword =.) <$> password
      , (BlogUrl =.) <$> url
      , (BlogTitle =.) <$> title
      ]

update' :: BlogId -> BlogForUpdate -> Handleable (Entity Blog)
update' key (BlogForUpdate hostUrl username password url title)
  = do
  blog' <- operate $ do
    blog' <- Blog.lookup key
    mapM (Blog.update updates) blog'
  case blog' of
    Just blog -> return blog
    Nothing   -> throwError err404
  where
    updates = catMaybes
      [ (BlogHostUrl =.) <$> hostUrl
      , (BlogUsername =.) <$> username
      , (BlogPassword =.) <$> password
      , (BlogUrl =.) <$> url
      , (BlogTitle =.) <$> title
      ]

destroy' :: BlogId -> Handleable NoContent
destroy' key = do
  blog' <- operate $ do
    blog' <- Blog.lookup key
    mapM Blog.destroy blog'
  case blog' of
    Just _  -> return NoContent
    Nothing -> throwError err404
