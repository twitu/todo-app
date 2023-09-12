module App.App where

import Servant
import qualified App.Server as S
import Network.Wai.Handler.Warp
import Storage.Types.App
import qualified Database.Redis as Redis
import qualified Data.Text.Encoding as DTE
import qualified Config.Types  as Conf
import qualified Config.Config as CC

app :: Env -> Application
app = serve S.todoProxy . S.todoServer

runServer :: IO ()
runServer = do
  conf <- CC.config
  let env = Env conf
  getKvConnection <- prepareKVConnection (Conf.kvConfig conf) (Conf.isRedisClusterEnabled conf)
  run (Conf.port conf) (app env)



prepareKVConnection :: Redis.ConnectInfo -> Bool -> IO ()
prepareKVConnection redisConf isClusterEnabled = do
  conn <- if  isClusterEnabled then Redis.connectCluster redisConf else Redis.connect redisConf
  Redis.runRedis conn $ Redis.set (DTE.encodeUtf8 "dummy_testing") "dummy"
  return ()

