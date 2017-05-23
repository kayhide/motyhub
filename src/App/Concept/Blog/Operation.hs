{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App.Concept.Blog.Operation where

import Prelude hiding (all, last)
import Data.Maybe
import Data.Time
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Database.Persist (Entity (..), Update, (=.))
import qualified Database.Persist as Persist
import Database.Persist.Relational
import Database.Relational.Query (relation, aggregateRelation, query, relationalQuery, relationalQuery', asc, desc, (!))
import qualified Database.Relational.Query as Query
import Language.SQL.Keyword

import Lib.Db
import Lib.Operation

import App.Model
import qualified App.Concept.Blog as Blog

all :: Operational [Entity Blog]
all = runResourceT $ runQuery (relationalQuery rel) () $$ CL.consume
  where
    rel = relation $ do
      u <- query Blog.blogs
      asc $ u ! Blog.id'
      return u

find :: BlogId -> Operational (Entity Blog)
find key = do
  Just user <- runResourceT $ runQuery Blog.selectBlogs key $$ CL.head
  return $ user

create :: [Update Blog] -> Operational (Entity Blog)
create updates = do
  now <- liftIO getCurrentTime
  let record = build (updates ++ [BlogCreatedAt =. now, BlogUpdatedAt =. now])
  key <- Persist.insert $ record
  return $ Entity key record

update :: [Update Blog] -> (Entity Blog) -> Operational (Entity Blog)
update updates (Entity key record) = do
  now <- liftIO getCurrentTime
  let updates' = updates ++ [BlogUpdatedAt =. now]
  Persist.update key updates'
  return $ Entity key $ apply updates' record

destroy :: (Entity Blog) -> Operational ()
destroy (Entity key _) = do
  Persist.delete key

reload :: (Entity Blog) -> Operational (Entity Blog)
reload (Entity key _) = find key

first :: Operational (Maybe (Entity Blog))
first = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  where
    rel = relation $ do
      u <- query Blog.blogs
      asc $ u ! Blog.id'
      return u
    suffix = [LIMIT, "1"]

last :: Operational (Maybe (Entity Blog))
last = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  where
    rel = relation $ do
      u <- query Blog.blogs
      desc $ u ! Blog.id'
      return u
    suffix = [LIMIT, "1"]

count :: Operational Int
count = fmap fromJust $ runResourceT $ runQuery (relationalQuery rel) () $$ CL.head
  where
    rel = aggregateRelation $ do
      u <- query Blog.blogs
      return $ Query.count $ u ! Blog.id'

page :: Int -> Int -> Operational [Entity Blog]
page offset limit = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.consume
  where
    rel = relation $ do
      u <- query Blog.blogs
      asc $ u ! Blog.id'
      return u
    suffix = ["OFFSET", word (show offset), ROWS, LIMIT, word (show limit)]
