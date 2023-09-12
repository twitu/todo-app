module Storage.KV.KVConf where

import qualified Database.Redis as R

kvConnectionInfo :: R.ConnectInfo
kvConnectionInfo = R.ConnInfo "localhost" (R.PortNumber 6379) (Just "123456") 0 50 30 Nothing Nothing

kvGetConnection :: IO (R.Connection)
kvGetConnection = R.connect kvConnectionInfo