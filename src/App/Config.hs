{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Config where

import Control.Lens
import GHC.Generics

import App.Config.Application
import App.Config.Db

data FullSetting = FullSetting
  { fullSettingApplication :: ApplicationSetting
  , fullSettingDb :: DbSetting
  } deriving (Show, Generic)

makeFields ''FullSetting

data FullRunning = FullRunning
  { fullRunningApplication :: ApplicationRunning
  , fullRunningDb :: DbRunning
  } deriving (Show, Generic)

makeFields ''FullRunning

data Config = Config
  { configSetting :: FullSetting
  , configRunning :: FullRunning
  } deriving (Show, Generic)

makeFields ''Config
