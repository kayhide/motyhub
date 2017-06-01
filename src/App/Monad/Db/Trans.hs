{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Monad.Db.Trans where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import App.Prelude

newtype AppDbT m a = AppDbT { unAppDbT :: IdentityT m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadBase b
    , MonadIO
    , MonadThrow
    , MonadTrans
    )

runAppDbT :: AppDbT m a -> m a
runAppDbT = runIdentityT . unAppDbT

instance MonadTransControl AppDbT where
  type StT AppDbT a = a
  liftWith f = lift (f runAppDbT)
  restoreT = AppDbT . IdentityT
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (AppDbT m) where
  type StM (AppDbT m) a = ComposeSt AppDbT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadResource m => MonadResource (AppDbT m) where
  liftResourceT = lift . liftResourceT
