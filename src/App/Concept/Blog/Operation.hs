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

import Mid.Db.Query

import App.Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Article as Article
import Mid.Db.Monad as Db


all :: (MonadMidDb m) => m [Entity Blog]
all = all_ >>= asc_ Blog.id' & Db.queryMany

lookup :: (MonadMidDb m) => BlogId -> m (Maybe (Entity Blog))
lookup key = all_ >>= lookup_ key & Db.queryOne

find :: (MonadMidDb m) => BlogId -> m (Entity Blog)
find key = fromJust <$> lookup key

create :: (MonadMidDb m, MonadIO m) => Changeset Blog -> m (Entity Blog)
create changeset = do
  now <- liftIO getCurrentTime
  Db.create $ changeset ++ [BlogCreatedAt =. now, BlogUpdatedAt =. now]

update :: (MonadMidDb m, MonadIO m) => Entity Blog -> Changeset Blog -> m (Entity Blog)
update blog changeset = do
  now <- liftIO getCurrentTime
  Db.update blog $ changeset ++ [BlogUpdatedAt =. now]

destroy :: (MonadMidDb m) => Entity Blog -> m ()
destroy = Db.destroy

reload :: (MonadMidDb m) => Entity Blog -> m (Entity Blog)
reload (Entity key _) = find key

first :: (MonadMidDb m) => m (Maybe (Entity Blog))
first = all_ >>= asc_ Blog.id' & Db.queryOne

last :: (MonadMidDb m) => m (Maybe (Entity Blog))
last = all_ >>= desc_ Blog.id' & Db.queryOne

count :: (MonadMidDb m) => m Int
count = all_ >>= count_ Blog.id' & Db.queryCounted


all_ :: MonadQuery m => m (Projection Flat Blog.Blogs)
all_ = query Blog.blogs

lookup_ :: MonadRestrict Flat m
        => BlogId -> Projection Flat Blog.Blogs -> m (Projection Flat Blog.Blogs)
lookup_ key = where_ Blog.id' (.=.) key

articles_ :: (MonadQuery m, MonadRestrict Flat m)
          => Entity Blog -> m (Projection Flat Article.Articles)
articles_ (Entity blogId _) = query Article.articles >>= where_ Article.blogId' (.=.) blogId
