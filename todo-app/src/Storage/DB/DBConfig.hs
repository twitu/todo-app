module Storage.DB.DBConfig where

import Database.Beam.Postgres as BP
import qualified Flow as F
import qualified Config.Types  as Conf
import Control.Monad.IO.Class

dbGetConnection :: F.Flow (BP.Connection)
dbGetConnection =  do
  conf <- F.getConfig
  liftIO $ BP.connect $ Conf.dbConfig conf