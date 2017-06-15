module Main (main) where

import Data.Default
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import System.Environment
import Network.Wai
import Database.Persist (Entity (..), toJsonText, deleteWhere, (=.), (!=.))
import Database.Persist.Sql (runSqlPool)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Mid.Db.Monad
import Mid.Db.Config
import App.Config as Config
import App.Model as Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Blog.Operation as Blog
import App.Prelude
import App (makeApp)
import Dev

main :: IO ()
main = do
  setEnv "APP_ENV" "test"
  config <- Config.boot
  hspec $ before_ (flushDb config) $ spec $ makeApp config

flushDb :: Config -> IO ()
flushDb config = do
  runSqlPool sql pool'
  where
    pool' = config ^. dbConfig . running . pool
    sql = do
      deleteWhere [BlogId !=. def]
      deleteWhere [ArticleId !=. def]

spec :: Application -> Spec
spec app = with (return app) $ do
  describe "GET /blogs" $ do
    it "responds with 200" $ do
      get "/blogs" `shouldRespondWith` 200
    it "responds with [Entity Blog]" $ do
      e@(Entity _ blog) <- liftIO $ runDb
        $ Blog.create [ BlogUrl =. "http://somewhere.com/"
                      , BlogTitle =. "Something"
                      ]
      let body = "" { matchBody = MatchBody (\_ actual -> diff actual) }
          diff x = diff' x $ do
            [Entity _ x] :: [Entity Blog] <- Aeson.decode x
            guard $ (x ^. Model.url) == "http://somewhere.com/"
            guard $ (x ^. Model.title) == "Something"
          diff' x Nothing = Just $ "body mismatch:"
                            ++ "\n  expected: " ++ (show . toJsonText) [e]
                            ++ "\n    actual: " ++ show x
          diff' x _ = Nothing
      get "/blogs" `shouldRespondWith` body
