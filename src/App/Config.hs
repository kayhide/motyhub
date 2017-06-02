{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Config
  ( module App.Config
  , module Lib.Config
  ) where

import Control.Lens
import GHC.Generics

import Lib.Config
import App.Config.Application
import App.Config.Db


data Config = Config
  { configApplication :: ApplicationConfig
  , configDb :: DbConfig
  } deriving (Show, Generic)

makeFields ''Config

instance HasApplicationConfig Config where
  applicationConfig = application

instance HasDbConfig Config where
  dbConfig = db
