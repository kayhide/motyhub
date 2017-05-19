{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App.Monad.Operable where

import Control.Monad.Base
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Monad.Trans.Control
       (MonadBaseControl(StM, liftBaseWith, restoreM))
import Control.Natural ((:~>)(NT))
import Database.Persist.Sql
import Servant

import App.Config

newtype Operable a = Operable
  { unOperatable :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadBase IO
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             )


-- | The 'MonadBaseControl' instance for 'Operable' is required for using
-- 'runSqlPool' in 'runDb'.  It is somewhat complicated and can safely be
-- ignored if you've never seen it before.
instance MonadBaseControl IO Operable where
  type StM Operable a = Either ServantErr a

  liftBaseWith
    :: forall a.
       ((forall x. Operable x -> IO (Either ServantErr x)) -> IO a) -> Operable a
  liftBaseWith f =
    Operable $
    ReaderT $ \r ->
      ExceptT $
      fmap Right $
      f $ \(Operable readerTExceptT) -> runExceptT $ runReaderT readerTExceptT r

  restoreM :: forall a. Either ServantErr a -> Operable a
  restoreM eitherA = Operable . ReaderT . const . ExceptT $ pure eitherA


-- | Run a Persistent query.
runDb :: ReaderT SqlBackend Operable a -> Operable a
runDb query = do
  pool <- reader configPool
  runSqlPool query pool
