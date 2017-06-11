{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mid.Db.Monad.Class where

import Data.Maybe
import Data.Default
import Control.Monad.Trans (MonadTrans)
import Database.Persist.Sql hiding (insert, update, delete)
import Database.Persist.Relational
import Database.Relational.Query hiding (Update, set)

import Mid.Db.Prelude

type Changeset v = [Update v]

class ( PersistEntityBackend record ~ SqlBackend
      , PersistEntity record
      ) => DbEntity record
instance ( PersistEntityBackend record ~ SqlBackend
         , PersistEntity record
         ) => DbEntity record

data QueryRange = Limit Int | Offset Int | LimitOffset Int Int | PagePer Int Int

class Monad m => MonadMidDb m where

  queryMany
    :: (ToPersistEntity a1 a)
    => QuerySimple (Projection Flat a1) -> m [a]
  default queryMany
    :: ( ToPersistEntity a1 a
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => QuerySimple (Projection Flat a1) -> m [a]
  queryMany = lift . queryMany

  querySome
    :: (ToPersistEntity a1 a)
    => QueryRange -> QuerySimple (Projection Flat a1) -> m [a]
  default querySome
    :: ( ToPersistEntity a1 a
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => QueryRange -> QuerySimple (Projection Flat a1) -> m [a]
  querySome = (lift .) . querySome

  queryOne
    :: (ToPersistEntity a1 a)
    => QuerySimple (Projection Flat a1) -> m (Maybe a)
  default queryOne
    :: ( ToPersistEntity a1 a
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => QuerySimple (Projection Flat a1) -> m (Maybe a)
  queryOne = lift . queryOne

  queryAggregated
    :: (ToPersistEntity a1 a)
    => QueryAggregate (Projection Aggregated a1) -> m (Maybe a)
  default queryAggregated
    :: ( ToPersistEntity a1 a
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => QueryAggregate (Projection Aggregated a1) -> m (Maybe a)
  queryAggregated = lift . queryAggregated

  queryCounted
    :: (ToPersistEntity a1 a, a ~ Int)
    => QueryAggregate (Projection Aggregated a1) -> m Int
  queryCounted proj = fmap fromJust $ queryAggregated proj
  -- default queryCounted
  --   :: ( ToPersistEntity a1 a
  --      , a ~ Int
  --      , MonadMidDb n
  --      , MonadTrans t
  --      , m ~ t n
  --      )
  --   => QueryAggregate (Projection Aggregated a1) -> m Int
  -- queryCounted = lift . queryCounted


  create
    :: (DbEntity record, Default record)
    => Changeset record -> m (Entity record)
  default create
    :: ( DbEntity record
       , Default record
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => Changeset record -> m (Entity record)
  create = lift . create

  update
    :: (DbEntity record)
    => Entity record -> Changeset record -> m (Entity record)
  default update
    :: ( DbEntity record
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => Entity record -> Changeset record -> m (Entity record)
  update = (lift .) . update

  destroy
    :: (DbEntity record)
    => Entity record -> m ()
  default destroy
    :: ( DbEntity record
       , MonadMidDb n
       , MonadTrans t
       , m ~ t n
       )
    => Entity record -> m ()
  destroy = lift . destroy


instance MonadMidDb m => MonadMidDb (ReaderT r m)
