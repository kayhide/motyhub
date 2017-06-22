
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
  Get :: URI -> CrawlerI Response
  Submit :: Form -> CrawlerI Response
  Click :: Link -> CrawlerI Response


type Crawler a = Program CrawlerI a

runCrawler :: Crawler a -> IO a
runCrawler m = Session.withSession $ \session -> runCrawler' session m

runCrawler' :: Session -> Crawler a -> IO a
runCrawler' session = eval session . view
  where
    eval :: Session -> ProgramView CrawlerI a -> IO a

    eval _ (Return x) = return x

    eval session (Get url :>>= k) =
      Session.get session (show url) >>= runCrawler' session . k

    eval session (Submit form :>>= k) =
      Session.post session url' xs' >>= runCrawler' session . k
      where
        url' = form ^. action . to show
        xs' = uncurry (:=) <$> form ^. fields . to Map.toList

    eval session (Click link :>>= k) =
      Session.get session url' >>= runCrawler' session . k
      where
        url' = link ^. href . to show


get :: URI -> Crawler Response
get = singleton . Get

submitFrom :: URI -> Form -> Crawler Response
submitFrom url form = singleton $ Submit form'
  where
    form' = form & action %~ flip relativeTo url

clickFrom :: URI -> Link -> Crawler Response
clickFrom url link = singleton $ Click link'
  where
    link' = link & href %~ flip relativeTo url
