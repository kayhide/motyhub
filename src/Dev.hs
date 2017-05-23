{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Dev where

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Database.Record.ToSql
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Relational
import Database.Relational.Query

import Lib.Config as Config
import Lib.Operation
import App.Config.Db
import App.Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Blog.Operation as Blog

run :: Operational a -> IO a
run sql = do
  dbSetting :: DbSetting <- Config.current
  let info = Text.encodeUtf8 $ buildConnectionInfo dbSetting
  runStderrLoggingT $ withPostgresqlConn info $ runSqlConn sql

runRelation :: ToPersistEntity a1 a => Relation () a1 -> IO [a]
runRelation rel = runRelationWith rel ()

runRelationWith :: (ToSql PersistValue p, ToPersistEntity a1 a)
                => Relation p a1 -> p -> IO [a]
runRelationWith rel p = run $ runResourceT $ runQuery (relationalQuery rel) p $$ CL.consume
