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

import Mid.Db.Query

import App.Model
import qualified App.Concept.Article as Article
import qualified App.Concept.Blog as Blog
import Mid.Db.Monad as Db


all :: (MonadMidDb m) => m [Entity Article]
all = all_ >>= asc_ Article.id' & Db.queryMany

lookup :: (MonadMidDb m) => ArticleId -> m (Maybe (Entity Article))
lookup key = all_ >>= lookup_ key & Db.queryOne

find :: (MonadMidDb m) => ArticleId -> m (Entity Article)
find key = fromJust <$> lookup key

create :: (MonadMidDb m, MonadIO m) => Changeset Article -> m (Entity Article)
create changeset = do
  now <- liftIO getCurrentTime
  Db.create $ changeset ++ [ArticleCreatedAt =. now, ArticleUpdatedAt =. now]

update :: (MonadMidDb m, MonadIO m) => Entity Article -> Changeset Article -> m (Entity Article)
update article changeset = do
  now <- liftIO getCurrentTime
  Db.update article $ changeset ++ [ArticleUpdatedAt =. now]

destroy :: (MonadMidDb m) => Entity Article -> m ()
destroy = Db.destroy

reload :: (MonadMidDb m) => Entity Article -> m (Entity Article)
reload (Entity key _) = find key

first :: (MonadMidDb m) => m (Maybe (Entity Article))
first = all_ >>= asc_ Article.id' & Db.queryOne

last :: (MonadMidDb m) => m (Maybe (Entity Article))
last = all_ >>= desc_ Article.id' & Db.queryOne

count :: (MonadMidDb m) => m Int
count = all_ >>= count_ Article.id' & Db.queryCounted


allOf :: (MonadMidDb m) => BlogId -> m [Entity Article]
allOf blogId = all_ >>= asc_ Article.id' >>= where_ Article.blogId' (.=.) blogId & Db.queryMany

lookupOf :: (MonadMidDb m) => BlogId -> ArticleId -> m (Maybe (Entity Article))
lookupOf blogId key = all_ >>= where_ Article.blogId' (.=.) blogId >>= lookup_ key & Db.queryOne


all_ :: MonadQuery m => m (Projection Flat Article.Articles)
all_ = query Article.articles

lookup_ :: MonadRestrict Flat m
        => ArticleId -> Projection Flat Article.Articles -> m (Projection Flat Article.Articles)
lookup_ key = where_ Article.id' (.=.) key

blog_ :: (HasBlogId t BlogId, MonadRestrict Flat m, MonadQuery m)
      => Entity t -> m (Projection Flat Blog.Blogs)
blog_ (Entity _ record) = query Blog.blogs >>= where_ Blog.id' (.=.) (record ^. blogId)
