
module Mid.Crawler.Lib
  ( run
  ) where

import Data.Maybe
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LazyText
import Text.Xml.Lens
import Network.URI
import Network.Wreq.Lens (responseBody)
import Control.Arrow
import Control.Lens
import System.Environment

import Mid.Crawler.Monad
import Mid.Crawler.Type
import Mid.Crawler.Lens

run :: IO ()
run = do
  Just url <- parseURI <$> getEnv "TRY_CRAWLER_URL"
  username <- Text.pack <$> getEnv "TRY_CRAWLER_USERNAME"
  password <- Text.pack <$> getEnv "TRY_CRAWLER_PASSWORD"

  putStrLn "trying crawler..."
  res <- runCrawler $ do
    res <- get url
    let Just form = res ^? responseBody . forms
        form' = form
                & fields . at "username" .~ Just username
                & fields . at "password" .~ Just password

    res <- submit form'
    let Just link =
          res ^? responseBody . links
          . filtered (isInfixOf "__mode=cfg_prefs" . uriQuery . view href)
          . filtered (not . isInfixOf "blog_id=1" . uriQuery . view href)

    res <- click link
    return res
  mapM_ print' $ res ^.. responseBody . html
  mapM_ printForm' $ res ^.. responseBody . forms
  -- mapM_ printLink' $ res ^.. responseBody . links


-- * For debug

print' :: Element -> IO ()
print' x = LazyText.putStr $ x ^. renderWith (rsPretty .~ True)

printForm' :: Form -> IO ()
printForm' form = do
  print $ form ^. action
  print $ form ^. fields
  -- print' $ form ^. dom
  print $ form ^? domId
  print $ form ^.. domClass
  putStrLn ""

printLink' :: Link -> IO ()
printLink' link = do
  print $ link ^. href
  Text.putStrLn $ link ^. anchor
  print' $ link ^. dom
  print $ link ^? domId
  print $ link ^.. domClass
  putStrLn ""
