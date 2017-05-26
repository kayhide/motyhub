{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Concept.Article.Serializer
  (ArticleForCreate(..), ArticleForUpdate(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Database.Persist
import GHC.Generics

import Lib.Serializer
import App.Model
import qualified App.Concept.Article as Article


instance ToJSON Article where
  toJSON = genericToJSON jsonOptions

instance FromJSON Article where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON (Entity Article) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Article) where
  parseJSON = entityIdFromJSON


data ArticleForCreate = ArticleForCreate
  { articleforcreateTitle :: Maybe Text
  , articleforcreateBody :: Maybe Text
  , articleforcreateBasename :: Maybe (Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON ArticleForCreate where
  toJSON = genericToJSON jsonOptions

instance FromJSON ArticleForCreate where
  parseJSON = genericParseJSON jsonOptions


data ArticleForUpdate = ArticleForUpdate
  { articleforupdateBlogId :: Maybe BlogId
  , articleforupdateTitle :: Maybe Text
  , articleforupdateBody :: Maybe Text
  , articleforupdateBasename :: Maybe (Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON ArticleForUpdate where
  toJSON = genericToJSON jsonOptions

instance FromJSON ArticleForUpdate where
  parseJSON = genericParseJSON jsonOptions
