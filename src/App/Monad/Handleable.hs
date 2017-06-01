{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App.Monad.Handleable where

import Control.Monad.Base
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control
       (MonadBaseControl(StM, liftBaseWith, restoreM))
import Database.Persist.Sql
import Servant

import Lib.Operation
import App.Config (Config(..))
import qualified App.Config as Config
import qualified App.Config.Db as Config
import App.Monad.Db

newtype Handleable a = Handleable
  { unHandleable :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadBase IO
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             , MonadThrow
             )


-- | The 'MonadBaseControl' instance for 'Handleable' is required for using
-- 'runSqlPool' in 'runDb'.  It is somewhat complicated and can safely be
-- ignored if you've never seen it before.
instance MonadBaseControl IO Handleable where
  type StM Handleable a = Either ServantErr a

  liftBaseWith
    :: forall a.
       ((forall x. Handleable x -> IO (Either ServantErr x)) -> IO a) -> Handleable a
  liftBaseWith f =
    Handleable $
    ReaderT $ \r ->
      ExceptT $
      fmap Right $
      f $ \(Handleable readerTExceptT) -> runExceptT $ runReaderT readerTExceptT r

  restoreM :: forall a. Either ServantErr a -> Handleable a
  restoreM eitherA = Handleable . ReaderT . const . ExceptT $ pure eitherA


operate :: Operational a -> Handleable a
operate op = do
  pool <- reader $ (Config.dbRunningPool . Config.fullRunningDb . Config.configRunning)
  runSqlPool op pool

runDb :: AppDbT Handleable a -> Handleable a
runDb = runAppDbT

verifyPresence :: Maybe a -> Handleable a
verifyPresence (Just x) = return x
verifyPresence Nothing  = throwError err404
