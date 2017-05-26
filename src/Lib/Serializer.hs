{-# LANGUAGE MultiParamTypeClasses #-}

module Lib.Serializer where

import qualified Data.Text as Text
import Data.Time
import Data.Default
import Data.Aeson
import Data.Aeson.Types
import Text.Inflections
import Database.Persist (PersistEntity, Update)


jsonOptions = defaultOptions { fieldLabelModifier = modifier }
  where
    modifier src = either (const src) id $ do
      words <- parseCamelCase [] $ Text.pack src
      return $ Text.unpack $ underscore $ tail words

class (PersistEntity v) => ToChangeset v a where
  toChangeset :: a -> [Update v]
