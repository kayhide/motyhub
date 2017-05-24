{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Concept.Blog.Serializer
  (BlogForCreate(..), BlogForUpdate(..)
  ) where

import Data.Either
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types
import Text.Inflections
import Database.Persist
import GHC.Generics

import App.Model
import qualified App.Concept.Blog as Blog

labelModifier :: String -> String
labelModifier src = either (const src) id $ do
  words <- parseCamelCase [] $ Text.pack src
  return $ Text.unpack $ underscore $ tail words

options = defaultOptions { fieldLabelModifier = labelModifier }

instance ToJSON Blog where
  toJSON = genericToJSON options

instance FromJSON Blog where
  parseJSON = genericParseJSON options

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
  toJSON = genericToJSON options

instance FromJSON BlogForCreate where
  parseJSON = genericParseJSON options


data BlogForUpdate = BlogForUpdate
  { blogforupdateHostUrl :: Maybe Text
  , blogforupdateUsername :: Maybe Text
  , blogforupdatePassword :: Maybe Text
  , blogforupdateUrl :: Maybe Text
  , blogforupdateTitle :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON BlogForUpdate where
  toJSON = genericToJSON options

instance FromJSON BlogForUpdate where
  parseJSON = genericParseJSON options
