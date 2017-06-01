{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad.Trans.Control
import Database.Record.ToSql
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Relational
import Database.Relational.Query hiding (Config)

import Lib.Config as Config
import Lib.Operation
import App.Prelude
import App.Config
import App.Config.Application
import App.Config.Db
import App.Model
import App.Monad.Db
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Blog.Operation as Blog
import qualified App.Concept.Article as Article
import qualified App.Concept.Article.Operation as Article

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



newtype Dev a = Dev { unDev :: ReaderT Config IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadBase IO
           , MonadIO
           , MonadReader Config
           , MonadThrow
           )

instance MonadBaseControl IO Dev where
  type StM Dev a = a
  liftBaseWith f = Dev $ liftBaseWith $ \q -> f (q . unDev)
  restoreM = Dev . restoreM

setup :: IO Config
setup = do
  applicationSetting :: ApplicationSetting <- Config.current
  applicationRunning <- Config.initialize applicationSetting
  dbSetting :: DbSetting <- Config.current
  dbRunning <- Config.initialize dbSetting
  let
    port = applicationRunningPort applicationRunning
    config = Config
      (FullSetting applicationSetting dbSetting)
      (FullRunning applicationRunning dbRunning)
  return config

runDb :: AppDbT Dev a -> IO a
runDb sql = do
  conf <- setup
  flip runReaderT conf $ unDev $ runAppDbT sql
