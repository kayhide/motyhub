module App.Concept.Blog.Crawling.Handler
  ( handlers
  ) where

import Data.Maybe
import Data.List
import Data.Aeson
import Data.Aeson.Lens
import Control.Monad
import Control.Lens
import Database.Persist
import Servant

import Mid.Db.Monad

import App.Prelude
import App.Model
import App.Monad.Handleable
import qualified App.Concept.Blog.Operation as Blog
import qualified App.Concept.Blog.Crawling.Operation as Blog
import qualified App.Concept.Article.Operation as Article
import App.Concept.Blog.Serializer
import App.Concept.Article.Serializer


handlers = create'


create' :: BlogId -> Handleable NoContent
create' blogId = do
  blog' <- runDb $ Blog.lookup blogId
  blog <- verifyPresence blog'
  items <- liftIO $ Blog.crawl blog
  articles <- runDb $ Blog.articles_ blog & queryMany
  let f = articleforcreateBasename
      g = Just . view basename . entityVal
      pred x y = f x == g y
      changesets = do
        x <- toArticleForCreate <$> items
        return ((ArticleBlogId =. blogId) : toChangeset x, find (pred x) articles)

  mapM_ createOrUpdate changesets

  runDb $ Blog.update blog []
  return NoContent

toArticleForCreate :: Value -> ArticleForCreate
toArticleForCreate x = ArticleForCreate
                       (x ^? key "TITLE" . _String)
                       (x ^? key "BODY" . _String)
                       (x ^? key "BASENAME" . _String)

createOrUpdate :: (Changeset Article, Maybe (Entity Article)) -> Handleable (Entity Article)
createOrUpdate (changeset, Nothing) = runDb $ Article.create changeset
createOrUpdate (changeset, Just article) = runDb $ Article.update article changeset
