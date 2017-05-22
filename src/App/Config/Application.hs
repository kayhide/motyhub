{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Config.Application where

import Data.Maybe
import qualified Data.Map as Map
import Data.Monoid
import Data.Aeson.Lens
import Data.Default
import Control.Applicative
import Control.Lens.Operators
import System.FilePath
import System.Environment
import GHC.Generics
import Network.Wai.Handler.Warp (Port)

import Lib.Config

data ApplicationConfig
type ApplicationSetting = Setting ApplicationConfig
type ApplicationRunning = Running ApplicationConfig

instance Default ApplicationSetting

instance Monoid ApplicationSetting where
  mempty = def
  mappend x y = ApplicationSetting
    (applicationSettingPort x <|> applicationSettingPort y)

instance Configurable ApplicationConfig where
  data Setting ApplicationConfig = ApplicationSetting
    { applicationSettingPort :: Maybe Port
    } deriving (Show, Generic)

  data Running ApplicationConfig = ApplicationRunning
    { applicationRunningPort :: Port
    } deriving (Show, Generic)

  initialize setting = return $ ApplicationRunning port
    where
      port = fromJust $ applicationSettingPort setting

  readConfigFile = do
    Just vals <- readYaml configFile
    return $ Map.fromList $ pick <$> vals ^@.. members
      where
        pick (env, vals) = (,) env $ ApplicationSetting
          { applicationSettingPort = vals ^? key "port" . _Integral
          }

  readEnvVars = Just <$> do
    port' <- lookupEnv "APPLICATION_PORT"
    return $ def
      { applicationSettingPort = fmap read port'
      }

configFile :: FilePath
configFile = "config" </> "application.yaml"
