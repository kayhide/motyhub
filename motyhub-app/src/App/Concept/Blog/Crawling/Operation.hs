module App.Concept.Blog.Crawling.Operation
  ( crawl
  , try
  ) where

import Data.Monoid
import Data.List
import qualified Data.Map as Map
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
import Control.Lens
import Database.Persist
import Network.URI
import Network.Wreq.Lens (responseBody)
import System.FilePath
import System.Directory

import Mid.Crawler.Monad as Crawler
import Mid.Crawler.Type hiding (key)
import Mid.Crawler.Lens

import qualified Lib.Movable as Movable
import App.Prelude
import App.Model


crawl :: Entity Blog -> IO [Value]
crawl blog@(Entity _ record) = do
  doc <- liftIO $ runCrawler $ crawl' blog
  return $ Movable.parseItems doc

crawl' :: Entity Blog -> Crawler LazyText.Text
crawl' (Entity _ record) = do
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

  return $ LazyText.decodeUtf8 $ res ^. responseBody


-- * For debug

try :: Entity Blog -> IO ()
try blog = do
  putStrLn "trying Movable..."
  doc <- read'
  let articles = Movable.parseItems doc
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

    read' :: IO LazyText.Text
    read' = do
      let file = "tmp" </> "blog-" ++ (blog & entityKey & toJsonText & Text.unpack) </> "articles.txt"
      exists <- doesFileExist file
      when (not exists) $ void $ do
        createDirectoryIfMissing True (takeDirectory file)
        LazyText.writeFile file =<< runCrawler (crawl' blog)
      LazyText.readFile file
