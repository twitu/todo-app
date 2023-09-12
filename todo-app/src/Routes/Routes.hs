{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Routes where

import Servant
import qualified Storage.Types.API as SA
import qualified Flow as F
import qualified Controller.Todo as CT
import Data.Text
import Storage.Types.App
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Exception

type TodoAPIs = "todo" :>
  ("create" :> ReqBody '[JSON] SA.CreateTodoRequest :> Post '[JSON] (SA.CreateTodoResponse)
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "fetchAll" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "getDetails" :> Capture "taskName" String :> Get '[JSON] String )
 
type ApplicationAPIs = "application" :> "app" :> Get '[JSON,PlainText] String

type UserAPIs = "user" :>
  ("create" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String)
   
type APIs = TodoAPIs  :<|> UserAPIs :<|> ApplicationAPIs

-- TODO

createTodo :: SA.CreateTodoRequest  -> FlowHandler (SA.CreateTodoResponse)
createTodo req =  lift $ ExceptT $ try $ runReaderT (CT.createTask req) $ Env "temp"

updateTodo :: String-> FlowHandler String
updateTodo = return 

fetchAllTodo :: String -> FlowHandler String
fetchAllTodo = return

getDetailsTodo :: String ->  FlowHandler String
getDetailsTodo = return

-- APPLICATION
appCheck :: FlowHandler String
appCheck =  return "Todo App is Running"

-- USER

createUser :: String -> FlowHandler String
createUser = return

updateUser :: String -> FlowHandler String
updateUser = return
