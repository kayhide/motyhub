{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib.Operation
  ( Operational
  , Changeset
  , build
  , apply
  , create
  , update
  , destroy
  , takeAll
  , takeSome
  , takeFrom
  , takeSomeFrom
  , takeOne
  , takeAggregated
  , takeCounted
  , module Lib.Query
  ) where

import Data.Maybe
import Data.Default
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Lens
import Database.Persist.Sql hiding (insert, update, delete)
import qualified Database.Persist.Sql as Persist
import Database.Persist.Relational
import Database.Relational.Query hiding (Update, set)
import Language.SQL.Keyword

import Lib.Query


type Operational a = forall m. ( Monad m
                               , MonadIO m
                               , MonadBaseControl IO m
                               , MonadThrow m
                               ) => SqlPersistT m a

type Changeset v = [Update v]

build :: (PersistEntity v, Default v) => Changeset v -> v
build = flip apply def

apply :: (PersistEntity v) => Changeset v -> v -> v
apply updates record = foldr apply' record updates
  where
    apply' :: (PersistEntity v) => Update v -> v -> v
    apply' (Update f x Assign) record = entityVal $ set (fieldLens f) x (Entity undefined record)
    apply' _ _ = error $ "apply supports only `Update` of `Assign`"


create :: ( Default record
          , PersistEntityBackend record ~ SqlBackend
          , PersistEntity record
          )
       => Changeset record
       -> Operational (Entity record)
create changeset = do
  let record = build changeset
  key <- Persist.insert $ record
  return $ Entity key record

update :: ( PersistEntityBackend record ~ SqlBackend
          , PersistEntity record
          )
       => Entity record
       -> Changeset record
       -> Operational (Entity record)
update (Entity key record) changeset = do
  let record' = apply changeset record
  Persist.update key changeset
  return $ Entity key record'

destroy :: ( PersistEntityBackend record ~ SqlBackend
           , PersistEntity record
           )
        => Entity record
        -> Operational ()
destroy (Entity key _) = Persist.delete key


takeAll :: (ToPersistEntity a1 a)
        => QuerySimple (Projection Flat a1)
        -> Operational [a]
takeAll proj = runResourceT $ runQuery (relationalQuery rel) () $$ CL.consume
  where rel = relation proj

takeSome :: (Integral i, Show i, ToPersistEntity a1 a)
         => i
         -> QuerySimple (Projection Flat a1)
         -> Operational [a]
takeSome limit proj = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.consume
  where
    rel = relation proj
    suffix = [LIMIT, word (show limit)]

takeFrom :: (Integral i, Show i, ToPersistEntity a1 a)
         => i
         -> QuerySimple (Projection Flat a1)
         -> Operational [a]
takeFrom offset proj = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.consume
  where
    rel = relation proj
    suffix = ["OFFSET", word (show offset)]

takeSomeFrom :: (Integral i, Show i, ToPersistEntity a1 a)
             => i
             -> i
             -> QuerySimple (Projection Flat a1)
             -> Operational [a]
takeSomeFrom limit offset proj = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.consume
  where
    rel = relation proj
    suffix = [LIMIT, word (show limit), "OFFSET", word (show offset)]

takeOne :: (ToPersistEntity a1 a)
        => QuerySimple (Projection Flat a1)
        -> Operational (Maybe a)
takeOne proj = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  where
    rel = relation proj
    suffix = [LIMIT, "1"]

takeAggregated :: (ToPersistEntity a1 a)
               => QueryAggregate (Projection Aggregated a1)
               -> Operational (Maybe a)
takeAggregated proj = runResourceT $ runQuery (relationalQuery' rel suffix) () $$ CL.head
  where
    rel = aggregateRelation proj
    suffix = [LIMIT, "1"]

takeCounted :: (ToPersistEntity a1 a, a ~ Int)
            => QueryAggregate (Projection Aggregated a1)
            -> Operational Int
takeCounted proj = fmap fromJust $ takeAggregated proj
