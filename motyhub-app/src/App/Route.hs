module App.Route where

import Database.Persist
import Servant

import App.Model
import App.Concept.Blog.Serializer
import App.Concept.Article.Serializer

type API =
  "blogs" :>
  (      Get '[JSON] [Entity Blog]
    :<|> Capture "blog_id" BlogId :> Get '[JSON] (Entity Blog)
    :<|> ReqBody '[JSON] BlogForCreate :> Post '[JSON] (Entity Blog)
    :<|> Capture "blog_id" BlogId :> ReqBody '[JSON] BlogForUpdate :> Patch '[JSON] (Entity Blog)
    :<|> Capture "blog_id" BlogId :> DeleteNoContent '[JSON] NoContent
  ) :<|>
  "blogs" :> Capture "blog_id" BlogId :> "crawling" :> PostAccepted '[JSON] NoContent
    :<|>
  "blogs" :> Capture "blog_id" BlogId :> "articles" :>
  (      Get '[JSON] [Entity Article]
    :<|> Capture "article_id" ArticleId :> Get '[JSON] (Entity Article)
    :<|> ReqBody '[JSON] ArticleForCreate :> Post '[JSON] (Entity Article)
    :<|> Capture "article_id" ArticleId :> ReqBody '[JSON] ArticleForUpdate :> Patch '[JSON] (Entity Article)
    :<|> Capture "article_id" ArticleId :> DeleteNoContent '[JSON] NoContent
  )
