{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App.Concept.Blog.Operation where

import Prelude hiding (all, lookup, last)
import Data.Function
import Data.Maybe
import Data.Time
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Database.Persist (Entity(..), (=.))
import qualified Database.Persist as Persist
import Database.Persist.Relational
import Database.Relational.Query

import Lib.Query

import App.Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Article as Article
import App.Monad.Db as Db


all :: (MonadAppDb m) => m [Entity Blog]
all = all_ >>= asc_ Blog.id' & Db.queryMany

lookup :: (MonadAppDb m) => BlogId -> m (Maybe (Entity Blog))
lookup key = all_ >>= lookup_ key & Db.queryOne

find :: (MonadAppDb m) => BlogId -> m (Entity Blog)
find key = fromJust <$> lookup key

create :: (MonadAppDb m, MonadIO m) => Changeset Blog -> m (Entity Blog)
create changeset = do
  now <- liftIO getCurrentTime
  Db.create $ changeset ++ [BlogCreatedAt =. now, BlogUpdatedAt =. now]

update :: (MonadAppDb m, MonadIO m) => (Entity Blog) -> Changeset Blog -> m (Entity Blog)
update blog changeset = do
  now <- liftIO getCurrentTime
  Db.update blog $ changeset ++ [BlogUpdatedAt =. now]

destroy :: (MonadAppDb m) => (Entity Blog) -> m ()
destroy = Db.destroy

reload :: (MonadAppDb m) => (Entity Blog) -> m (Entity Blog)
reload (Entity key _) = find key

first :: (MonadAppDb m) => m (Maybe (Entity Blog))
first = all_ >>= asc_ Blog.id' & Db.queryOne

last :: (MonadAppDb m) => m (Maybe (Entity Blog))
last = all_ >>= desc_ Blog.id' & Db.queryOne

count :: (MonadAppDb m) => m Int
count = all_ >>= count_ Blog.id' & Db.queryCounted


all_ :: MonadQuery m => m (Projection Flat Blog.Blogs)
all_ = query Blog.blogs

lookup_ :: MonadRestrict Flat m
        => BlogId -> Projection Flat Blog.Blogs -> m (Projection Flat Blog.Blogs)
lookup_ key = where_ Blog.id' (.=.) key

articles_ :: (MonadQuery m, MonadRestrict Flat m)
          => Entity Blog -> m (Projection Flat Article.Articles)
articles_ (Entity blogId _) = query Article.articles >>= where_ Article.blogId' (.=.) blogId
