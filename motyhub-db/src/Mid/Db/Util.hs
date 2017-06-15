module Mid.Db.Util where

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)

type DbConnectionCount = Int

makePoolFromUrl :: DbConnectionCount -> ConnectionString -> IO ConnectionPool
makePoolFromUrl n connectionString =
  runStdoutLoggingT $ createPostgresqlPool connectionString n
