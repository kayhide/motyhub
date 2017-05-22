{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
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
import System.Environment

type AppEnv = Text
type SettingMap a = Map AppEnv (Setting a)

currentEnv :: IO AppEnv
currentEnv = Text.pack <$> fromMaybe "development" <$> lookupEnv "APP_ENV"

readYaml :: FilePath -> IO (Maybe Yaml.Value)
readYaml file = Yaml.decodeFile file

class Configurable a where
  data Setting a
  data Running a

  initialize :: Setting a -> IO (Running a)

  readConfigFile :: IO (SettingMap a)
  readConfigFile = return Map.empty

  readEnvVars :: IO (Maybe (Setting a))
  readEnvVars = return Nothing

current :: (Configurable a, Monoid (Setting a)) => IO (Setting a)
current = do
  env <- currentEnv
  whole' <- whole
  let x = fromMaybe mempty $ Map.lookup "envs" whole'
      y = whole' ! env
  return $ (x <> y)

pick :: (Configurable a) => AppEnv -> IO (Setting a)
pick env = (! env) <$> whole

whole :: (Configurable a) => IO (SettingMap a)
whole = do
  x <- maybe Map.empty (Map.singleton "envs") <$> readEnvVars
  Map.union x <$> readConfigFile
