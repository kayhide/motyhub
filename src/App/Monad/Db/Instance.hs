{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Monad.Db.Instance where

import Data.Maybe
import Data.Default
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Lens
import Control.Monad.Trans.Resource
import Database.Persist.Sql hiding (insert, update, delete)
import qualified Database.Persist.Sql as Persist
import Database.Persist.Relational
import Database.Relational.Query hiding (Update, set)
import Language.SQL.Keyword

import App.Prelude
import App.Monad.Db.Class
import App.Monad.Db.Trans
import App.Config
import App.Config.Db


run' :: ( MonadIO m
        , MonadBaseControl IO m
        , MonadReader App.Config.Config m
        , MonadThrow m
        ) => SqlPersistT m a -> m a
run' sql = do
  pool <- reader $ (dbRunningPool . fullRunningDb . configRunning)
  runSqlPool sql pool

build :: (PersistEntity v, Default v) => Changeset v -> v
build = flip apply def

apply :: (PersistEntity v) => Changeset v -> v -> v
apply updates record = foldr apply' record updates
  where
    apply' :: (PersistEntity v) => Update v -> v -> v
    apply' (Update f x Assign) record = entityVal $ set (fieldLens f) x (Entity undefined record)
    apply' _ _ = error $ "apply supports only `Update` of `Assign`"

instance ( MonadIO m
         , MonadBaseControl IO m
         , MonadReader App.Config.Config m
         , MonadThrow m
         ) => MonadAppDb (AppDbT m) where

  queryMany proj = lift $ run' $ do
    runResourceT $ runQuery (relationalQuery rel) () $$ CL.consume
    where
      rel = relation proj

  querySome range proj = lift $ run' $ do
    runResourceT $ runQuery (relationalQuery' rel (suffix range)) () $$ CL.consume
    where
      rel = relation proj
      suffix (Limit limit) = [LIMIT, word (show limit)]
      suffix (Offset offset) = ["OFFSET", word (show offset)]
      suffix (LimitOffset limit offset) = [LIMIT, word (show limit), "OFFSET", word (show offset)]
      suffix (PagePer page per) = [LIMIT, word (show per), "OFFSET", word (show (page * per))]

  queryOne proj = lift $ run' $ do
    runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
    where
      rel = relation proj
      suffix = [LIMIT, "1"]

  queryAggregated proj = lift $ run' $ do
    runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
    where
      rel = aggregateRelation proj
      suffix = [LIMIT, "1"]

  -- queryCounted proj = fmap fromJust $ lift $ run' $ do
  --   runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  --   where
  --     rel = aggregateRelation proj
  --     suffix = [LIMIT, "1"]

  create changeset = lift $ run' $ do
    let record = build changeset
    key <- Persist.insert $ record
    return $ Entity key record

  update (Entity key record) changeset = lift $ run' $ do
    let record' = apply changeset record
    Persist.update key changeset
    return $ Entity key record'

  destroy (Entity key _) = lift $ run' $ Persist.delete key
