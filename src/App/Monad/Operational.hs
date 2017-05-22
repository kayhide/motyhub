{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App.Monad.Operational where

import Control.Monad.Base
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Monad.Trans.Control
       (MonadBaseControl(StM, liftBaseWith, restoreM))
import Control.Lens
import Database.Persist.Sql
import Servant

import App.Config (Config(..))
import qualified App.Config as Config
import qualified App.Config.Db as Config

newtype Operational a = Operational
  { unOperational :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadBase IO
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             )


-- | The 'MonadBaseControl' instance for 'Operational' is required for using
-- 'runSqlPool' in 'runDb'.  It is somewhat complicated and can safely be
-- ignored if you've never seen it before.
instance MonadBaseControl IO Operational where
  type StM Operational a = Either ServantErr a

  liftBaseWith
    :: forall a.
       ((forall x. Operational x -> IO (Either ServantErr x)) -> IO a) -> Operational a
  liftBaseWith f =
    Operational $
    ReaderT $ \r ->
      ExceptT $
      fmap Right $
      f $ \(Operational readerTExceptT) -> runExceptT $ runReaderT readerTExceptT r

  restoreM :: forall a. Either ServantErr a -> Operational a
  restoreM eitherA = Operational . ReaderT . const . ExceptT $ pure eitherA


-- | Run a Persistent query.
runDb :: ReaderT SqlBackend Operational a -> Operational a
runDb query = do
  pool <- reader $ (Config.dbRunningPool . Config.fullRunningDb . Config.configRunning)
  runSqlPool query pool
