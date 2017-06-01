{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Monad.Db.Instance where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Database.Persist.Sql hiding (insert, update, delete)
import qualified Database.Persist.Sql as Persist
import Database.Persist.Relational
import Database.Relational.Query hiding (Update, set)

import App.Prelude
import App.Monad.Db.Class
import App.Monad.Db.Trans
import App.Config
import App.Config.Db


runSql :: ( MonadIO m
          , MonadBaseControl IO m
          , MonadReader App.Config.Config m
          , MonadThrow m
          ) => SqlPersistT m a -> m a
runSql sql = do
  pool <- reader $ (dbRunningPool . fullRunningDb . configRunning)
  runSqlPool sql pool


instance ( MonadIO m
         , MonadBaseControl IO m
         , MonadReader App.Config.Config m
         , MonadThrow m
         ) => MonadAppDb (AppDbT m) where

  dbTakeAll :: (ToPersistEntity a1 a)
            => QuerySimple (Projection Flat a1)
            -> AppDbT m [a]
  dbTakeAll proj = lift $ runSql sql
    where
      sql = runResourceT $ runQuery (relationalQuery rel) () $$ CL.consume
      rel = relation proj
