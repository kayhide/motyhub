module App.Concept.Blog.Crawling.Handler
  ( handlers
  ) where

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Text.Xml.Lens
import Control.Monad
import Control.Arrow
import Control.Lens
import Database.Persist
import Network.URI
import Network.Wreq.Lens (responseBody)
import Servant

import Mid.Db.Monad
import Mid.Crawler.Monad as Crawler
import Mid.Crawler.Type
import Mid.Crawler.Lens

import App.Prelude
import App.Model
import App.Monad.Handleable
import qualified App.Concept.Blog.Operation as Blog
import App.Concept.Blog.Serializer

import Debug.Trace as Debug

handlers = create'


create' :: BlogId -> Handleable NoContent
create' blogId = do
  blog' <- runDb $ Blog.lookup blogId
  blog <- verifyPresence blog'
  liftIO $ runCrawler $ crawling' blog
  return NoContent


crawling' :: Entity Blog -> Crawler ()
crawling' (Entity _ record) = do
  let Just url' = parseURI $ Text.unpack $ record ^. hostUrl
      username' = record ^. username
      password' = record ^. password

  res <- Crawler.get url'


  let Just form = res ^? responseBody . html . forms
      form' = form
              & fields . at "username" .~ Just username'
              & fields . at "password" .~ Just password'
  res <- submit form'

  let Just link =
        res ^? responseBody . html . links
        . filtered (isInfixOf "__mode=cfg_prefs" . uriQuery . view href)
        . filtered (not . isInfixOf "blog_id=1" . uriQuery . view href)
  res <- click link

  let Just siteUrl = res ^? responseBody . html . selected "#view-site" . links . href
  let Just ext = res ^? responseBody . html . selected "input#file_extension" . _Input . value
  -- Debug.traceShow siteUrl $ return ()
  -- Debug.traceShow ext $ return ()

  let Just link =
        res ^? responseBody . html . links
        . filtered (isInfixOf "__mode=start_export" . uriQuery . view href)
  res <- click link

  let Just form =
        res ^? responseBody . html . forms
        . filtered ((== Just "export") . Map.lookup "__mode" . view fields)
  res <- submit form

  Debug.trace (res ^. responseBody . to LazyText.decodeUtf8 . to LazyText.unpack) $ return ()
  return ()
