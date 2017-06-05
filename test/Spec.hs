{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Control.Monad
import Control.Lens
import System.Environment
import Network.Wai
import Database.Persist (Entity (..), toJsonText, deleteWhere, (=.), (!=.))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Lib.Config as Config
import App.Config
import Mid.Db.Config
import App.Model as Model
import qualified App.Concept.Blog as Blog
import qualified App.Concept.Blog.Operation as Blog
import App.Prelude
import App (makeApp)
import Dev (run)

main :: IO ()
main = do
  setEnv "APP_ENV" "test"
  putStr $ "running in: "
  print =<< Config.currentEnv

  dbSetting :: DbSetting <- Config.current
  dbRunning <- Config.initialize dbSetting
  let
    config = Config
      (FullSetting undefined dbSetting)
      (FullRunning undefined dbRunning)
  hspec $ before_ flushDb $ spec $ makeApp config

flushDb :: IO ()
flushDb = run $ deleteWhere [BlogId !=. def]

spec :: Application -> Spec
spec app = with (return app) $ do
  describe "GET /blogs" $ do
    it "responds with 200" $ do
      get "/blogs" `shouldRespondWith` 200
    it "responds with [Entity Blog]" $ do
      e@(Entity _ blog) <- liftIO $ run
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
