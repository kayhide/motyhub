{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module App.Concept.Blog.Serializer
  ( BlogForCreate(..)
  , BlogForUpdate(..)
  , toChangeset
  ) where

import Data.Maybe
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

instance ToChangeset Blog BlogForCreate where
  toChangeset BlogForCreate {..} =
    catMaybes [ (BlogHostUrl =.) <$> blogforcreateHostUrl
              , (BlogUsername =.) <$> blogforcreateUsername
              , (BlogPassword =.) <$> blogforcreatePassword
              , (BlogUrl =.) <$> blogforcreateUrl
              , (BlogTitle =.) <$> blogforcreateTitle
              ]


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

instance ToChangeset Blog BlogForUpdate where
  toChangeset BlogForUpdate {..} =
    catMaybes [ (BlogHostUrl =.) <$> blogforupdateHostUrl
              , (BlogUsername =.) <$> blogforupdateUsername
              , (BlogPassword =.) <$> blogforupdatePassword
              , (BlogUrl =.) <$> blogforupdateUrl
              , (BlogTitle =.) <$> blogforupdateTitle
              ]
