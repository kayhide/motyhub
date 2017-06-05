{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Mid.Db.Config
  ( module Mid.Db.Config
  , module Lib.Config
  ) where

import Data.Maybe
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Monoid
import Data.Aeson.Lens
import Data.Default
import Control.Applicative
import Control.Lens
import Control.Lens.TH
import System.FilePath
import System.Environment
import GHC.Generics
import Database.Persist.Sql (ConnectionPool)

import Lib.Config
import Mid.Db.Util as Db


class HasDbConfig a where
  dbConfig :: Lens' a DbConfig

data DbSetting = DbSetting
  { settingHost :: Maybe Text
  , settingPort :: Maybe Int
  , settingDatabase :: Maybe Text
  , settingUsername :: Maybe Text
  , settingPassword :: Maybe Text
  , settingEncoding :: Maybe Text
  , settingUrl :: Maybe Text
  , settingPool :: Maybe Int
  } deriving (Show, Generic, Default)

data DbRunning = DbRunning
  { runningPool :: !ConnectionPool
  } deriving (Show, Generic)

type DbConfig = (DbSetting, DbRunning)

makeLensesWith abbreviatedFields ''DbSetting
makeLensesWith abbreviatedFields ''DbRunning

instance Monoid DbSetting where
  mempty = def
  mappend x y = DbSetting
    (settingHost x <|> settingHost y)
    (settingPort x <|> settingPort y)
    (settingDatabase x <|> settingDatabase y)
    (settingUsername x <|> settingUsername y)
    (settingPassword x <|> settingPassword y)
    (settingEncoding x <|> settingEncoding y)
    (settingUrl x <|> settingUrl y)
    (settingPool x <|> settingPool y)

instance ConfigurableSetting DbSetting where
  readConfigFile = do
    Just vals <- readYaml configFile
    return $ Map.fromList $ pick <$> vals ^@.. members
      where
        configFile = "config" </> "db.yaml"
        pick (env, vals) = (,) env $ DbSetting
          { settingHost = vals ^? key "host" . _String
          , settingPort = vals ^? key "port" . _Integral
          , settingDatabase = vals ^? key "database" . _String
          , settingUsername = vals ^? key "username" . _String
          , settingPassword = vals ^? key "password" . _String
          , settingEncoding = vals ^? key "encoding" . _String
          , settingUrl = vals ^? key "url" . _String
          , settingPool = vals ^? key "pool" . _Integral
          }

  readEnvVars = Just <$> do
    url' <- lookupEnv "DATABASE_URL"
    pool' <- lookupEnv "DATABASE_POOL"
    return $ def
      { settingUrl = fmap Text.pack url'
      , settingPool = fmap read pool'
      }

instance ConfigurableRunning DbRunning

instance Configurable DbSetting DbRunning where
  initialize setting = DbRunning <$> Db.makePoolFromUrl count url
    where
      count = fromJust $ settingPool setting
      url = Text.encodeUtf8 $ buildConnectionInfo setting


buildConnectionInfo :: DbSetting -> Text
buildConnectionInfo conf = fromMaybe info $ settingUrl conf
  where
    info = Text.unwords $ catMaybes
      [ ("host=" <>) <$> settingHost conf
      , ("port=" <>) . Text.pack . show <$> settingPort conf
      , ("dbname=" <>) <$> settingDatabase conf
      , ("user=" <>) <$> settingUsername conf
      , ("password=" <>) <$> settingPassword conf
      ]
