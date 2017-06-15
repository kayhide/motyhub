{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Config.Application where

import Data.Maybe
import qualified Data.Map as Map
import Data.Monoid
import Data.Aeson.Lens
import Data.Default
import Control.Applicative
import Control.Lens
import System.FilePath
import System.Environment
import GHC.Generics
import Network.Wai.Handler.Warp (Port)

import Lib.Config


class HasApplicationConfig a where
  applicationConfig :: Lens' a ApplicationConfig

data ApplicationSetting = ApplicationSetting
  { settingPort :: Maybe Port
  } deriving (Show, Generic, Default)

data ApplicationRunning = ApplicationRunning
  { runningPort :: Port
  } deriving (Show, Generic)

type ApplicationConfig = (ApplicationSetting, ApplicationRunning)

makeLensesWith abbreviatedFields ''ApplicationSetting
makeLensesWith abbreviatedFields ''ApplicationRunning

instance Monoid ApplicationSetting where
  mempty = def
  mappend x y = ApplicationSetting
    (settingPort x <|> settingPort y)

instance ConfigurableSetting ApplicationSetting where
  readConfigFile = do
    Just vals <- readYaml configFile
    return $ Map.fromList $ pick <$> vals ^@.. members
      where
        pick (env, vals) = (,) env ApplicationSetting
          { settingPort = vals ^? key "port" . _Integral
          }

  readEnvVars = Just <$> do
    port' <- lookupEnv "APPLICATION_PORT"
    return $ def
      { settingPort = fmap read port'
      }

instance ConfigurableRunning ApplicationRunning

instance Configurable ApplicationSetting ApplicationRunning where
  initialize setting = return $ ApplicationRunning $ fromJust $ settingPort setting

configFile :: FilePath
configFile = "config" </> "application.yaml"
