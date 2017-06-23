module Mid.Crawler.Lib
  ( run
  ) where

import Data.Monoid
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
  -- mapM_ print' $ res ^.. responseBody . html
  mapM_ printForm' $ res ^.. responseBody . forms
  -- mapM_ printLink' $ res ^.. responseBody . links


-- * For debug

print' :: Element -> IO ()
print' x = LazyText.putStr $ x ^. renderWith (rsPretty .~ True)

printForm' :: Form -> IO ()
printForm' form = do
  Text.putStrLn $ mconcat
    [ form ^. dom . name
    , form ^. domId . to ("#" <>)
    , form ^. domClass . to ("." <>)
    ]
  print $ form ^. action
  print $ form ^. fields
  -- print' $ form ^. dom
  putStrLn ""

printLink' :: Link -> IO ()
printLink' link = do
  Text.putStrLn $ mconcat
    [ link ^. dom . name
    , link ^. domId . to ("#" <>)
    , link ^. domClass . to ("." <>)
    ]
  print $ link ^. href
  Text.putStrLn $ link ^. innerText
  -- print' $ link ^. dom
  putStrLn ""
