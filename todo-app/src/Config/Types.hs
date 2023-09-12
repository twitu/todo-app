module Config.Types where

import Database.Beam.Postgres as BP
import qualified Database.Redis as R

data Config =
  Config 
    {
      port :: Int,
      dbConfig :: BP.ConnectInfo,
      isRedisClusterEnabled :: Bool,
      kvConfig ::  R.ConnectInfo
    }
    deriving (Show)
