{-# LANGUAGE TypeFamilies #-}

module Dev where

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Database.Record.ToSql
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Relational
import Database.Relational.Query hiding (Config)

import Mid.Db.Query
import Mid.Db.Config
import Mid.Db.Monad
import App.Prelude
import App.Config as Config
import App.Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Blog.Operation as Blog
import qualified App.Concept.Blog.Crawling.Operation as Blog
import qualified App.Concept.Article as Article
import qualified App.Concept.Article.Operation as Article


newtype Dev a = Dev { unDev :: ReaderT Config IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadBase IO
           , MonadIO
           , MonadReader Config
           , MonadThrow
           )

instance MonadBaseControl IO Dev where
  type StM Dev a = a
  liftBaseWith f = Dev $ liftBaseWith $ \q -> f (q . unDev)
  restoreM = Dev . restoreM

runDb :: MidDbT Dev a -> IO a
runDb sql = do
  config <- Config.boot
  flip runReaderT config $ unDev $ runMidDbT sql
