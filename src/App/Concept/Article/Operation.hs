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

import Lib.Db
import Lib.Operation as Operation

import App.Model
import qualified App.Concept.Article as Article
import qualified App.Concept.Blog as Blog


all :: Operational [Entity Article]
all = all_ >>= asc_ Article.id' & takeAll

lookup :: ArticleId -> Operational (Maybe (Entity Article))
lookup key = all_ >>= lookup_ key & takeOne

find :: ArticleId -> Operational (Entity Article)
find key = fromJust <$> lookup key

create :: Changeset Article -> Operational (Entity Article)
create changeset = do
  now <- liftIO getCurrentTime
  Operation.create $ changeset ++ [ArticleCreatedAt =. now, ArticleUpdatedAt =. now]

update :: (Entity Article) -> Changeset Article -> Operational (Entity Article)
update article changeset = do
  now <- liftIO getCurrentTime
  Operation.update article $ changeset ++ [ArticleUpdatedAt =. now]

destroy :: (Entity Article) -> Operational ()
destroy = Operation.destroy

reload :: (Entity Article) -> Operational (Entity Article)
reload (Entity key _) = find key

first :: Operational (Maybe (Entity Article))
first = all_ >>= asc_ Article.id' & takeOne

last :: Operational (Maybe (Entity Article))
last = all_ >>= desc_ Article.id' & takeOne

count :: Operational Int
count = all_ >>= count_ Article.id' & takeCounted


allOf :: BlogId -> Operational [Entity Article]
allOf blogId = all_ >>= asc_ Article.id' >>= where_ Article.blogId' (.=.) blogId & takeAll

lookupOf :: BlogId -> ArticleId -> Operational (Maybe (Entity Article))
lookupOf blogId key = all_ >>= where_ Article.blogId' (.=.) blogId >>= lookup_ key & takeOne


all_ :: MonadQuery m => m (Projection Flat Article.Articles)
all_ = query Article.articles

lookup_ :: MonadRestrict Flat m
        => ArticleId -> Projection Flat Article.Articles -> m (Projection Flat Article.Articles)
lookup_ key = where_ Article.id' (.=.) key

blog_ :: (HasBlogId t BlogId, MonadRestrict Flat m, MonadQuery m)
      => Entity t -> m (Projection Flat Blog.Blogs)
blog_ (Entity _ record) = query Blog.blogs >>= where_ Blog.id' (.=.) (record ^. blogId)
