module Storage.KV.KVConf where

import qualified Database.Redis as R
import qualified Flow as F
import qualified Config.Types  as Conf
import Control.Monad.IO.Class

kvGetConnection :: F.Flow (R.Connection)
kvGetConnection =  do
  conf <- F.getConfig
  liftIO $ R.connect $ Conf.kvConfig conf