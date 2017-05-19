module App.Config where

import Database.Persist.Sql (ConnectionPool)
import Network.Wai.Handler.Warp (Port)

data Config = Config
  { configPool :: !ConnectionPool
  , configPort :: !Port
  }
