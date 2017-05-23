{-# LANGUAGE OverloadedStrings #-}

module App.Concept.Blog.Handler where

import Database.Persist
import Servant

import App.Model
import App.Monad.Handleable
import qualified App.Concept.Blog.Operation as Blog

handlers = index' :<|> create'

index' :: Handleable [Entity Blog]
index' = operate Blog.all

create' :: Handleable (Entity Blog)
create' = operate $ Blog.create [ BlogHostUrl =. "http://hoge.host/host"
                                , BlogUsername =. "tamura"
                                , BlogPassword =. "secret"
                                , BlogUrl =. "http://somewhere.com/"
                                , BlogTitle =. "Something"
                                ]
