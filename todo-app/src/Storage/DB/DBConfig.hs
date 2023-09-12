module Storage.DB.DBConfig where

import Database.Beam.Postgres as BP

dbConnectionInfo :: BP.ConnectInfo
dbConnectionInfo = BP.ConnectInfo "localhost" 5432 "cloud" "scape" "todo-db"

dbGetConnection :: IO (BP.Connection)
dbGetConnection =  BP.connect dbConnectionInfo