module External.DummyServer where

import Servant
import Servant.Client
import Servant.API
import qualified Network.HTTP.Client as HTTP

type SendAck = Get '[JSON] String

sendAckClient :: ClientM String
sendAckClient  = client (Proxy :: Proxy SendAck)

sendAck :: IO (Either String String)
sendAck = do
  let localhost = "127.0.0.1"
  let baseUrl = BaseUrl Http localhost 5000 "/taskCreated"
  manager <- HTTP.newManager HTTP.defaultManagerSettings --servant fucntion
  let clientEnv = mkClientEnv  manager baseUrl
  res <- runClientM sendAckClient clientEnv
  putStrLn $ show res
  case res of 
    Left err -> return .  Left $ "err while calling sendAck : " <> show err
    Right result ->  return $ Right result