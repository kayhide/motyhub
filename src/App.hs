{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App
    ( startApp
    , app
    ) where

import Control.Monad.Base
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Monad.Trans.Control
       (MonadBaseControl(StM, liftBaseWith, restoreM))
import Control.Natural ((:~>)(NT))
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist (Entity(entityVal), insert_, selectList)
import Database.Persist.Postgresql
       (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql
       (ConnectionPool, SqlBackend, runMigration, runSqlPool)
import Data.Proxy (Proxy(Proxy))
import Data.Time

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
       (Get, Handler(Handler), JSON, Post, ReqBody, ServantErr, Server, ServerT,
        (:>), (:<|>)((:<|>)), enter, serve)

import Model

data Config = Config
  { configPool :: !ConnectionPool  -- ^ A pool of database connections.
  , configPort :: !Port            -- ^ 'Port' to listen on.
  }

type DbPoolConnNum = Int

makePoolFromUrl
  :: DbPoolConnNum      -- ^ Number of database connections to use.
  -> ConnectionString
  -> IO ConnectionPool
makePoolFromUrl dbConnNum connectionString =
  runStdoutLoggingT $ createPostgresqlPool connectionString dbConnNum


-- | This 'MyApiM' monad is the monad that will be used for running the web
-- application handlers.  This includes 'helloWorldHandler',
-- 'addCommentHandler', and 'getCommentsHandler'.
--
-- It is just a newtype around @'ReaderT' 'Config' ('ExceptT' 'ServantErr' 'IO')@.
newtype MyApiM a = MyApiM
  { unMyApiM :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadBase IO
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             )

-- | The 'MonadBaseControl' instance for 'MyApiM' is required for using
-- 'runSqlPool' in 'runDb'.  It is somewhat complicated and can safely be
-- ignored if you've never seen it before.
instance MonadBaseControl IO MyApiM where
  type StM MyApiM a = Either ServantErr a

  liftBaseWith
    :: forall a.
       ((forall x. MyApiM x -> IO (Either ServantErr x)) -> IO a) -> MyApiM a
  liftBaseWith f =
    MyApiM $
    ReaderT $ \r ->
      ExceptT $
      fmap Right $
      f $ \(MyApiM readerTExceptT) -> runExceptT $ runReaderT readerTExceptT r

  restoreM :: forall a. Either ServantErr a -> MyApiM a
  restoreM eitherA = MyApiM . ReaderT . const . ExceptT $ pure eitherA

-- | Run a Persistent query.
runDb :: ReaderT SqlBackend MyApiM a -> MyApiM a
runDb query = do
  pool <- reader configPool
  runSqlPool query pool


type API = "blogs" :> Get '[JSON] [Blog]
           :<|>  "blogs" :> Post '[JSON] Blog

-- | Create a WAI 'Application' capable of running with Warp.
app :: Config -> Application
app config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: MyApiM :~> Handler
    naturalTrans = NT transformation

    -- This represents a natural transformation from 'MyApiM' to 'Handler'.
    -- This consists of unwrapping the 'MyApiM', running the
    -- @'ReaderT' 'Config'@, and wrapping the resulting value back up in a
    -- 'Handler'.
    transformation :: forall a . MyApiM a -> Handler a
    transformation = Handler . flip runReaderT config . unMyApiM


serverRoot :: ServerT API MyApiM
serverRoot = indexBlogs :<|> createBlog


indexBlogs :: MyApiM [Blog]
indexBlogs = do
  blogs <- runDb $ selectList [] []
  return $ entityVal <$> blogs

createBlog :: MyApiM Blog
createBlog = do
  now <- liftBase getCurrentTime
  let blog = Blog "http://hoge.host/host" "tamura" "secret" "http://somewhere.com/" "Something" now now
  key <- runDb $ insert_ blog
  return blog


startApp :: IO ()
startApp = do
  let url = "postgres://motyhub@localhost/motyhub_development"
      port = 8080
  connPool <- makePoolFromUrl 5 url
  let config = Config connPool port
  putStrLn $
    "running motyhub on port " <> show port <> "..."

  run port . logStdoutDev $ app config
