{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Concept.Blog.Serializer
  (BlogForCreate(..), BlogForUpdate(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Database.Persist
import GHC.Generics

import Lib.Serializer
import App.Model
import qualified App.Concept.Blog as Blog


instance ToJSON Blog where
  toJSON = genericToJSON jsonOptions

instance FromJSON Blog where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON (Entity Blog) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Blog) where
  parseJSON = entityIdFromJSON


data BlogForCreate = BlogForCreate
  { blogforcreateHostUrl :: Maybe Text
  , blogforcreateUsername :: Maybe Text
  , blogforcreatePassword :: Maybe Text
  , blogforcreateUrl :: Maybe Text
  , blogforcreateTitle :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON BlogForCreate where
  toJSON = genericToJSON jsonOptions

instance FromJSON BlogForCreate where
  parseJSON = genericParseJSON jsonOptions


data BlogForUpdate = BlogForUpdate
  { blogforupdateHostUrl :: Maybe Text
  , blogforupdateUsername :: Maybe Text
  , blogforupdatePassword :: Maybe Text
  , blogforupdateUrl :: Maybe Text
  , blogforupdateTitle :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON BlogForUpdate where
  toJSON = genericToJSON jsonOptions

instance FromJSON BlogForUpdate where
  parseJSON = genericParseJSON jsonOptions
