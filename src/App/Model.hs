{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Default
import Control.Lens
import Database.Persist.TH
import Database.Persist.Relational (mkHrrInstances)
import Database.Persist.TH
import GHC.Generics

import App.Prelude

share
  [mkPersist sqlSettings
  , mkSave "entityDefs"
  , mkHrrInstances
  ]
  [persistLowerCase|
Blog sql=blogs
  hostUrl  Text sql=host_url
  username Text
  password Text
  url      Text
  title    Text
  createdAt UTCTime sql=created_at
  updatedAt UTCTime sql=updated_at

  deriving Eq
  deriving Show
  deriving Generic
  |]

makeFields ''Blog


instance Default BlogId where
  def = BlogKey 0

instance Default Blog where
  def = Blog def def def def def def def
