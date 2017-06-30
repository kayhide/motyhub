module App.Concept.Blog.Crawling.Handler
  ( handlers
  , try
  ) where

import Data.Monoid
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Data.Time
import Data.Aeson
import Data.Aeson.Lens
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
import Mid.Crawler.Type hiding (key)
import Mid.Crawler.Lens

import Lib.Movable
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
  doc <- liftIO $ runCrawler $ crawling' blog
  return NoContent


crawling' :: Entity Blog -> Crawler LazyText.Text
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

  let Just siteUrl =
        res ^? responseBody . html . selected "#view-site" . links . href
  let Just ext =
        res ^? responseBody . html . selected "input#file_extension" . _Input . value
  let Just timezone =
        res ^? responseBody . html . selected "#server_offset"
        ... attributed (ix "selected" . only "selected") . attr "value" . folded

  let Just link =
        res ^? responseBody . html . links
        . filtered (isInfixOf "__mode=start_export" . uriQuery . view href)
  res <- click link

  let Just form =
        res ^? responseBody . html . forms
        . filtered ((== Just "export") . Map.lookup "__mode" . view fields)
  res <- submit form

  -- Debug.trace (res ^. responseBody . to LazyText.decodeUtf8 . to LazyText.unpack) $ return ()
  return $ LazyText.decodeUtf8 $ res ^. responseBody


try :: IO ()
try = do
  putStrLn "trying Movable..."
  doc <- LazyText.readFile "articles.txt"
  let articles = parseItems doc
  mapM_ print' articles
  putStrLn "done."
  where
    print' :: Value -> IO ()
    print' x = do
      Text.putStrLn $ "title: " <> x ^. key "TITLE" . _String
      Text.putStrLn $ "basename: " <> x ^. key "BASENAME" . _String
      Text.putStrLn $ "body: " <> x ^. key "BODY" . _String
      let zone = hoursToTimeZone 9
      let t' = x ^? key "DATE" . _JSON :: Maybe LocalTime
      putStr "date: "
      print $ t'
      case t' of
        Just t -> do
          putStr "date: "
          print $ localTimeToUTC zone t
      Text.putStrLn ""
