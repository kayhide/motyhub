{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Route where

import Database.Persist
import Servant

import App.Model
import App.Concept.Blog.Serializer

type API =
  "blogs" :>
  (      Get '[JSON] [Entity Blog]
    :<|> Capture "blogId" BlogId :> Get '[JSON] (Entity Blog)
    :<|> ReqBody '[JSON] BlogForCreate :> Post '[JSON] (Entity Blog)
    :<|> Capture "blogId" BlogId :> ReqBody '[JSON] BlogForUpdate :> Patch '[JSON] (Entity Blog)
    :<|> Capture "blogId" BlogId :> DeleteNoContent '[JSON] NoContent
  )
