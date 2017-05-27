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

import Lib.Db
import Lib.Operation as Operation

import App.Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Article as Article


all_ :: MonadQuery m => m (Projection Flat Blog.Blogs)
all_ = query Blog.blogs


all :: Operational [Entity Blog]
all = all_ >>= asc_ Blog.id' & takeAll

lookup :: BlogId -> Operational (Maybe (Entity Blog))
lookup key = all_ >>= where_ Blog.id' (.=.) key & takeOne

find :: BlogId -> Operational (Entity Blog)
find key = fromJust <$> lookup key

create :: Changeset Blog -> Operational (Entity Blog)
create changeset = do
  now <- liftIO getCurrentTime
  Operation.create $ changeset ++ [BlogCreatedAt =. now, BlogUpdatedAt =. now]

update :: (Entity Blog) -> Changeset Blog -> Operational (Entity Blog)
update blog changeset = do
  now <- liftIO getCurrentTime
  Operation.update blog $ changeset ++ [BlogUpdatedAt =. now]

destroy :: (Entity Blog) -> Operational ()
destroy = Operation.destroy

reload :: (Entity Blog) -> Operational (Entity Blog)
reload (Entity key _) = find key

first :: Operational (Maybe (Entity Blog))
first = all_ >>= asc_ Blog.id' & takeOne

last :: Operational (Maybe (Entity Blog))
last = all_ >>= desc_ Blog.id' & takeOne

count :: Operational Int
count = all_ >>= count_ Blog.id' & takeCounted


articles_ (Entity blogId _) = query Article.articles >>= where_ Article.blogId' (.=.) blogId
