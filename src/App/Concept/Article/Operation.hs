{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App.Concept.Article.Operation where

import Prelude hiding (all, lookup, last)
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
import qualified App.Concept.Article as Article

all :: Operational [Entity Article]
all = runResourceT $ runQuery (relationalQuery rel) () $$ CL.consume
  where
    rel = relation $ do
      u <- query Article.articles
      asc $ u ! Article.id'
      return u

lookup :: ArticleId -> Operational (Maybe (Entity Article))
lookup key = runResourceT $ runQuery Article.selectArticles key $$ CL.head

find :: ArticleId -> Operational (Entity Article)
find key = fromJust <$> lookup key

create :: [Update Article] -> Operational (Entity Article)
create updates = do
  now <- liftIO getCurrentTime
  let record = build (updates ++ [ArticleCreatedAt =. now, ArticleUpdatedAt =. now])
  key <- Persist.insert $ record
  return $ Entity key record

update :: [Update Article] -> (Entity Article) -> Operational (Entity Article)
update updates (Entity key record) = do
  now <- liftIO getCurrentTime
  let updates' = updates ++ [ArticleUpdatedAt =. now]
  Persist.update key updates'
  return $ Entity key $ apply updates' record

destroy :: (Entity Article) -> Operational ()
destroy (Entity key _) = do
  Persist.delete key

reload :: (Entity Article) -> Operational (Entity Article)
reload (Entity key _) = find key

first :: Operational (Maybe (Entity Article))
first = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  where
    rel = relation $ do
      u <- query Article.articles
      asc $ u ! Article.id'
      return u
    suffix = [LIMIT, "1"]

last :: Operational (Maybe (Entity Article))
last = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  where
    rel = relation $ do
      u <- query Article.articles
      desc $ u ! Article.id'
      return u
    suffix = [LIMIT, "1"]

count :: Operational Int
count = fmap fromJust $ runResourceT $ runQuery (relationalQuery rel) () $$ CL.head
  where
    rel = aggregateRelation $ do
      u <- query Article.articles
      return $ Query.count $ u ! Article.id'

page :: Int -> Int -> Operational [Entity Article]
page offset limit = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.consume
  where
    rel = relation $ do
      u <- query Article.articles
      asc $ u ! Article.id'
      return u
    suffix = ["OFFSET", word (show offset), ROWS, LIMIT, word (show limit)]
