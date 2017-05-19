{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.TH

share
  [mkPersist sqlSettings]
  [persistLowerCase|
Blog json sql=blogs
  hostUrl  Text sql=host_url
  username Text
  password Text
  url      Text
  title    Text
  createdAt UTCTime sql=created_at
  updatedAt UTCTime sql=updated_at

  deriving Eq
  deriving Show
  deriving Typeable
  |]

