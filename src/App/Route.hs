{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Route where

import Servant

import App.Model

type API = "blogs" :> Get '[JSON] [Blog]
  :<|>  "blogs" :> Post '[JSON] Blog
