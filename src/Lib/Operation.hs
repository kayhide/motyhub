{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Lib.Operation where

import Data.Time
import Data.Default
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Lens
import Database.Persist.Sql


type Operational a = forall m. ( Monad m
                               , MonadIO m
                               , MonadBaseControl IO m
                               , MonadThrow m
                               ) => SqlPersistT m a

build :: (PersistEntity v, Default v) => [Update v] -> v
build updates = apply updates def

apply :: (PersistEntity v) => [Update v] -> v -> v
apply updates record = foldr applyUpdate record updates

applyUpdate :: (PersistEntity v) => Update v -> v -> v
applyUpdate (Update f x Assign) record = entityVal $ set (fieldLens f) x (Entity undefined record)
applyUpdate _ _ = error $ "applyUpdate supports only `Update` of `Assign`"
