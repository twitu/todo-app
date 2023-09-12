module App.Server where

import qualified Routes.Routes as R
import Servant
import Storage.Types.App
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

type FlowServer = 
  ServerT R.APIs (ReaderT Env (ExceptT ServerError IO))

todoProxy :: Proxy R.APIs
todoProxy = Proxy

todoServer :: Env -> Server R.APIs
todoServer env = hoistServer todoProxy (f env ) todoServers'
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env' r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env'
      case eResult  of
        Left err -> undefined
        Right res -> pure res

todoServers' :: FlowServer        
todoServers' = todoAPI :<|> userAPI :<|> applicationAPI

todoAPI = R.createTodo :<|> R.updateTodo :<|> R.fetchAllTodo :<|> R.getDetailsTodo

userAPI = R.createUser :<|> R.updateUser

applicationAPI = R.appCheck

