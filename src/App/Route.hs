{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Route where

import Database.Persist
import Servant

import App.Model

type API = "blogs" :> Get '[JSON] [Entity Blog]
  :<|>  "blogs" :> Post '[JSON] (Entity Blog)
