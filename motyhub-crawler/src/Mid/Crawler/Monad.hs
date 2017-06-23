module Mid.Crawler.Monad where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Operational
-- import Control.Monad.Trans
import Control.Lens hiding (view)
import Network.URI
import Network.Wreq (FormParam((:=)))
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Session

import Mid.Crawler.Type


data CrawlerI a where
  CurrentUrl :: CrawlerI URI
  Get :: URI -> CrawlerI Response
  Submit :: Form -> CrawlerI Response
  Click :: Link -> CrawlerI Response


type Crawler a = Program CrawlerI a

runCrawler :: Crawler a -> IO a
runCrawler m = Session.withSession $ \session -> runCrawler' session url m
  where
    url = URI "" Nothing "" "" ""

runCrawler' :: Session -> URI -> Crawler a -> IO a
runCrawler' session url = eval session url . view
  where
    eval :: Session -> URI -> ProgramView CrawlerI a -> IO a

    eval _ _ (Return x) = return x

    eval session url (CurrentUrl :>>= k) =
      return url
      >>= runCrawler' session url . k

    eval session _ (Get url :>>= k) =
      Session.get session (show url)
      >>= runCrawler' session url . k

    eval session _ (Submit form :>>= k) =
      Session.post session (show uri') xs'
      >>= runCrawler' session uri' . k
      where
        uri' = form ^. action
        xs' = uncurry (:=) <$> form ^. fields . to Map.toList

    eval session _ (Click link :>>= k) =
      Session.get session (show uri')
      >>= runCrawler' session uri' . k
      where
        uri' = link ^. href

currentUrl :: Crawler URI
currentUrl = singleton CurrentUrl

get :: URI -> Crawler Response
get = singleton . Get

submit :: Form -> Crawler Response
submit form = do
  url <- currentUrl
  let form' = form & action %~ flip relativeTo url
  singleton $ Submit form'

click :: Link -> Crawler Response
click link = do
  url <- currentUrl
  let link' = link & href %~ flip relativeTo url
  singleton $ Click link'
