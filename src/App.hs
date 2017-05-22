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

import Data.Maybe
import Data.Time
import Data.Monoid
import Data.Proxy
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Natural ((:~>)(NT))
import Control.Lens
import Database.Persist (Entity (..), insert, selectList)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import Lib.Db as Db
import Lib.Config as Config
import App.Config
import App.Config.Application
import App.Config.Db
import App.Route
import App.Model

import App.Prelude
import App.Monad.Operational

app :: Config -> Application
app config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans server

    naturalTrans :: Operational :~> Handler
    naturalTrans = NT transformation

    transformation :: forall a . Operational a -> Handler a
    transformation = Handler . flip runReaderT config . unOperational


server :: ServerT API Operational
server = indexBlogs :<|> createBlog


indexBlogs :: Operational [Entity Blog]
indexBlogs = do
  blogs <- runDb $ selectList [] []
  return $ blogs

createBlog :: Operational (Entity Blog)
createBlog = do
  now <- liftIO getCurrentTime
  let blog = Blog "http://hoge.host/host" "tamura" "secret" "http://somewhere.com/" "Something" now now
  key <- runDb $ insert blog
  return $ Entity key blog

startApp :: IO ()
startApp = do
  applicationSetting <- Config.current :: IO ApplicationSetting
  applicationRunning <- Config.initialize applicationSetting
  dbSetting <- Config.current :: IO DbSetting
  dbRunning <- Config.initialize dbSetting
  let
    port = applicationRunningPort applicationRunning
    config = Config
      (FullSetting applicationSetting dbSetting)
      (FullRunning applicationRunning dbRunning)
  putStrLn $
    "running motyhub on port " <> show port <> "..."

  run port . logStdoutDev $ app config
