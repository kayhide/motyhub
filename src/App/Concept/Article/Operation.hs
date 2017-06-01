{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App.Concept.Article.Operation where

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
import Control.Lens
import Database.Persist (Entity(..), (=.))
import qualified Database.Persist as Persist
import Database.Persist.Relational
import Database.Relational.Query

import Lib.Query

import App.Model
import qualified App.Concept.Article as Article
import qualified App.Concept.Blog as Blog
import App.Monad.Db as Db


all :: (MonadAppDb m) => m [Entity Article]
all = all_ >>= asc_ Article.id' & Db.queryMany

lookup :: (MonadAppDb m) => ArticleId -> m (Maybe (Entity Article))
lookup key = all_ >>= lookup_ key & Db.queryOne

find :: (MonadAppDb m) => ArticleId -> m (Entity Article)
find key = fromJust <$> lookup key

create :: (MonadAppDb m, MonadIO m) => Changeset Article -> m (Entity Article)
create changeset = do
  now <- liftIO getCurrentTime
  Db.create $ changeset ++ [ArticleCreatedAt =. now, ArticleUpdatedAt =. now]

update :: (MonadAppDb m, MonadIO m) => (Entity Article) -> Changeset Article -> m (Entity Article)
update article changeset = do
  now <- liftIO getCurrentTime
  Db.update article $ changeset ++ [ArticleUpdatedAt =. now]

destroy :: (MonadAppDb m) => (Entity Article) -> m ()
destroy = Db.destroy

reload :: (MonadAppDb m) => (Entity Article) -> m (Entity Article)
reload (Entity key _) = find key

first :: (MonadAppDb m) => m (Maybe (Entity Article))
first = all_ >>= asc_ Article.id' & Db.queryOne

last :: (MonadAppDb m) => m (Maybe (Entity Article))
last = all_ >>= desc_ Article.id' & Db.queryOne

count :: (MonadAppDb m) => m Int
count = all_ >>= count_ Article.id' & Db.queryCounted


allOf :: (MonadAppDb m) => BlogId -> m [Entity Article]
allOf blogId = all_ >>= asc_ Article.id' >>= where_ Article.blogId' (.=.) blogId & Db.queryMany

lookupOf :: (MonadAppDb m) => BlogId -> ArticleId -> m (Maybe (Entity Article))
lookupOf blogId key = all_ >>= where_ Article.blogId' (.=.) blogId >>= lookup_ key & Db.queryOne


all_ :: MonadQuery m => m (Projection Flat Article.Articles)
all_ = query Article.articles

lookup_ :: MonadRestrict Flat m
        => ArticleId -> Projection Flat Article.Articles -> m (Projection Flat Article.Articles)
lookup_ key = where_ Article.id' (.=.) key

blog_ :: (HasBlogId t BlogId, MonadRestrict Flat m, MonadQuery m)
      => Entity t -> m (Projection Flat Blog.Blogs)
blog_ (Entity _ record) = query Blog.blogs >>= where_ Blog.id' (.=.) (record ^. blogId)
