module Config.Config where

import qualified System.Environment as SE
import qualified Data.Map as M
import qualified Config.Types  as Conf
import Database.Beam.Postgres as BP
import Text.Read
import Data.Maybe
import qualified Database.Redis as R
import qualified Data.Text.Encoding as DTE
import Data.ByteString.Internal
import qualified Data.Text as DT


config :: IO Conf.Config
config = do
  envMap <- M.fromList <$> SE.getEnvironment
  mkConfigFromEnv envMap

mkConfigFromEnv :: M.Map String String -> IO Conf.Config
mkConfigFromEnv envMap = 
  return $ 
    Conf.Config
      {
        port = (fromMaybe 8022 . (>>= readMaybe)) $ M.lookup "PORT" envMap,
        dbConfig = mkDbConfig envMap,
        isRedisClusterEnabled = fromMaybe False . (>>=readMaybe) $ M.lookup "REDIS_CLUSTERED_ENABLED" envMap,
        kvConfig =  mkKVConfig envMap
      }

mkDbConfig ::  M.Map String String -> BP.ConnectInfo
mkDbConfig envMap = do
  let maybeDbPassword = M.lookup "DB_PASSWORD" envMap
      dbPassword = maybe (error "db password is not present in env") id maybeDbPassword
  BP.ConnectInfo
      {
        connectHost = fromMaybe "localhost" $ M.lookup "DB_HOST" envMap,
        connectPort = (fromMaybe 5432 . (>>= readMaybe)) $ M.lookup "DB_PORT" envMap,
        connectUser = fromMaybe "cloud" $ M.lookup "DB_USER" envMap,
        connectPassword = dbPassword,
        connectDatabase = fromMaybe "todo-db" $ M.lookup "DB_NAME" envMap
      }

redisPassword :: M.Map String String -> Maybe ByteString
redisPassword envMap =
  case fromMaybe True .(>>=readMaybe) $ M.lookup "REDIS_PASSWORD_REQ" envMap of
    False -> Nothing
    _ -> Just $ DTE.encodeUtf8 $ DT.pack $ fromMaybe "123456" $ M.lookup "REDIS_AUTH" envMap
      
mkKVConfig ::M.Map String String -> R.ConnectInfo
mkKVConfig envMap = 
    R.ConnInfo
      {
        connectHost = fromMaybe "localhost" $ M.lookup "REDIS_HOST" envMap,
        connectPort = R.PortNumber $ (fromMaybe 6379 . (>>= readMaybe)) $ M.lookup "REDIS_PORT" envMap,
        connectAuth = redisPassword envMap,
        connectDatabase = 0,
        connectMaxConnections = 50,
        connectMaxIdleTime  = 30,
        connectTimeout = Nothing,
        connectTLSParams = Nothing
      } 