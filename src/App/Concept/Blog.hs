{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Concept.Blog where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Relational
import Database.Relational.Query
import GHC.Generics

import App.Model

$(defineTableFromPersistentWithConfig
   defaultConfig { schemaNameMode = SchemaNotQualified, identifierQuotation = Quotation '"' }
   "public"
   ''App.Model.Blog
   App.Model.entityDefs
 )
