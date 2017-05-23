{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Config.Db where

import Data.Maybe
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Monoid
import Data.Aeson.Lens
import Data.Default
import Control.Applicative
import Control.Lens.Operators
import Control.Lens.TH
import System.FilePath
import System.Environment
import GHC.Generics
import Database.Persist.Sql (ConnectionPool)

import Lib.Config
import Lib.Db as Db

data DbConfig
type DbSetting = Setting DbConfig
type DbRunning = Running DbConfig

instance Default DbSetting

instance Monoid DbSetting where
  mempty = def
  mappend x y = DbSetting
    (dbSettingHost x <|> dbSettingHost y)
    (dbSettingPort x <|> dbSettingPort y)
    (dbSettingDatabase x <|> dbSettingDatabase y)
    (dbSettingUsername x <|> dbSettingUsername y)
    (dbSettingPassword x <|> dbSettingPassword y)
    (dbSettingEncoding x <|> dbSettingEncoding y)
    (dbSettingUrl x <|> dbSettingUrl y)
    (dbSettingPool x <|> dbSettingPool y)

instance Configurable DbConfig where
  data Setting DbConfig = DbSetting
    { dbSettingHost :: Maybe Text
    , dbSettingPort :: Maybe Int
    , dbSettingDatabase :: Maybe Text
    , dbSettingUsername :: Maybe Text
    , dbSettingPassword :: Maybe Text
    , dbSettingEncoding :: Maybe Text
    , dbSettingUrl :: Maybe Text
    , dbSettingPool :: Maybe Int
    } deriving (Show, Generic)

  data Running DbConfig = DbRunning
    { dbRunningPool :: !ConnectionPool
    } deriving (Show, Generic)

  initialize setting = DbRunning <$> Db.makePoolFromUrl count url
    where
      count = fromJust $ dbSettingPool setting
      url = Text.encodeUtf8 $ buildConnectionInfo setting

  readConfigFile = do
    Just vals <- readYaml configFile
    return $ Map.fromList $ pick <$> vals ^@.. members
      where
        pick (env, vals) = (,) env $ DbSetting
          { dbSettingHost = vals ^? key "host" . _String
          , dbSettingPort = vals ^? key "port" . _Integral
          , dbSettingDatabase = vals ^? key "database" . _String
          , dbSettingUsername = vals ^? key "username" . _String
          , dbSettingPassword = vals ^? key "password" . _String
          , dbSettingEncoding = vals ^? key "encoding" . _String
          , dbSettingUrl = vals ^? key "url" . _String
          , dbSettingPool = vals ^? key "pool" . _Integral
          }

  readEnvVars = Just <$> do
    url' <- lookupEnv "DATABASE_URL"
    pool' <- lookupEnv "DATABASE_POOL"
    return $ def
      { dbSettingUrl = fmap Text.pack url'
      , dbSettingPool = fmap read pool'
      }

configFile :: FilePath
configFile = "config" </> "db.yaml"

buildConnectionInfo :: Setting DbConfig -> Text
buildConnectionInfo conf = fromMaybe info $ dbSettingUrl conf
  where
    info = Text.unwords $ catMaybes
      [ ("host=" <>) <$> dbSettingHost conf
      , ("port=" <>) . Text.pack . show <$> dbSettingPort conf
      , ("dbname=" <>) <$> dbSettingDatabase conf
      , ("user=" <>) <$> dbSettingUsername conf
      , ("password=" <>) <$> dbSettingPassword conf
      ]
