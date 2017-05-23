{-# LANGUAGE OverloadedStrings #-}

module App.Orphan where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Text
import Data.Default


instance Default Bool where
  def = False


instance Default Day where
  def = fromGregorian 0 0 0

instance Default DiffTime where
  def = secondsToDiffTime 0

instance Default UTCTime where
  def = UTCTime def def


instance Default Text where
  def = ""
