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
    , makeApp
    ) where

import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Aeson
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Natural ((:~>)(NT))
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
import App.Monad.Handleable
import qualified App.Concept.Blog.Handler as Blog
import qualified App.Concept.Article.Handler as Article

makeApp :: Config -> Application
makeApp config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans server

    naturalTrans :: Handleable :~> Handler
    naturalTrans = NT transformation

    transformation :: forall a . Handleable a -> Handler a
    transformation = Handler . flip runReaderT config . unHandleable

server :: ServerT API Handleable
server = Blog.handlers :<|> Article.handlers

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

  run port . logStdoutDev $ makeApp config
