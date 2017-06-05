{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mid.Db.Monad.Trans where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Mid.Db.Prelude

newtype MidDbT m a = MidDbT { unMidDbT :: IdentityT m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadBase b
    , MonadIO
    , MonadThrow
    , MonadTrans
    )

runMidDbT :: MidDbT m a -> m a
runMidDbT = runIdentityT . unMidDbT

instance MonadTransControl MidDbT where
  type StT MidDbT a = a
  liftWith f = lift (f runMidDbT)
  restoreT = MidDbT . IdentityT
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (MidDbT m) where
  type StM (MidDbT m) a = ComposeSt MidDbT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadResource m => MonadResource (MidDbT m) where
  liftResourceT = lift . liftResourceT
