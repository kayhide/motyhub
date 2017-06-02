{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Config where

import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Data.Monoid
import Control.Lens
import System.Environment

type AppEnv = Text
type SettingMap s = Map AppEnv s

currentEnv :: IO AppEnv
currentEnv = Text.pack <$> fromMaybe "development" <$> lookupEnv "APP_ENV"

readYaml :: FilePath -> IO (Maybe Yaml.Value)
readYaml file = Yaml.decodeFile file

class ConfigurableSetting setting where
  readConfigFile :: IO (SettingMap setting)
  readConfigFile = return Map.empty

  readEnvVars :: IO (Maybe setting)
  readEnvVars = return Nothing

class ConfigurableRunning running

class Configurable setting running where
  initialize :: setting -> IO running


setup :: ( Configurable setting running
         , ConfigurableSetting setting
         , ConfigurableRunning running
         , Monoid setting
         ) => IO (setting, running)
setup = do
  setting <- current
  running <- initialize setting
  return (setting, running)

current :: (ConfigurableSetting setting, Monoid setting) => IO setting
current = do
  env <- currentEnv
  whole' <- whole
  let x = fromMaybe mempty $ Map.lookup "envs" whole'
      y = whole' ! env
  return $ (x <> y)

pick :: (ConfigurableSetting setting) => AppEnv -> IO setting
pick env = (! env) <$> whole

whole :: (ConfigurableSetting setting) => IO (SettingMap setting)
whole = do
  x <- maybe Map.empty (Map.singleton "envs") <$> readEnvVars
  Map.union x <$> readConfigFile


setting :: (ConfigurableSetting s, ConfigurableRunning r) => Lens' (s, r) s
setting = _1

running :: (ConfigurableSetting s, ConfigurableRunning r) => Lens' (s, r) r
running = _2
