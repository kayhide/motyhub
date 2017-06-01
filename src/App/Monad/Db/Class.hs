{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Monad.Db.Class where

import Control.Monad.Trans (MonadTrans)
import Database.Persist.Sql hiding (insert, update, delete)
import qualified Database.Persist.Sql as Persist
import Database.Persist.Relational
import Database.Relational.Query hiding (Update, set)

import App.Prelude

class Monad m => MonadAppDb m where

  dbTakeAll
    :: (ToPersistEntity a1 a)
    => QuerySimple (Projection Flat a1)
    -> m [a]

  default dbTakeAll
    :: ( MonadAppDb n
       , MonadTrans t
       , m ~ t n
       , ToPersistEntity a1 a)
    => QuerySimple (Projection Flat a1)
    -> t n [a]

  dbTakeAll = lift . dbTakeAll


instance MonadAppDb m => MonadAppDb (ReaderT r m)
