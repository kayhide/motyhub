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

import Data.Time
import Data.Monoid
import Data.Proxy
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Natural ((:~>)(NT))
import Database.Persist (Entity (..), insert, selectList)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import Lib.Db as Db
import App.Config
import App.Route
import App.Model

import App.Prelude
import App.Monad.Operable

app :: Config -> Application
app config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans server

    naturalTrans :: Operable :~> Handler
    naturalTrans = NT transformation

    transformation :: forall a . Operable a -> Handler a
    transformation = Handler . flip runReaderT config . unOperatable


server :: ServerT API Operable
server = indexBlogs :<|> createBlog


indexBlogs :: Operable [Entity Blog]
indexBlogs = do
  blogs <- runDb $ selectList [] []
  return $ blogs

createBlog :: Operable (Entity Blog)
createBlog = do
  now <- liftIO getCurrentTime
  let blog = Blog "http://hoge.host/host" "tamura" "secret" "http://somewhere.com/" "Something" now now
  key <- runDb $ insert blog
  return $ Entity key blog


startApp :: IO ()
startApp = do
  let url = "postgres://motyhub@localhost/motyhub_development"
      port = 8080
  connPool <- Db.makePoolFromUrl 5 url
  let config = Config connPool port
  putStrLn $
    "running motyhub on port " <> show port <> "..."

  run port . logStdoutDev $ app config
