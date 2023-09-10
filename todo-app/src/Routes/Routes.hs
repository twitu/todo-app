{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Routes where

import Servant

type TodoAPIs = "todo" :>
  ("create" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "fetchAll" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "getDetails" :> ReqBody '[JSON] String :> Post '[JSON] String)
 
type ApplicationAPIs = "application" :> "app" :> Get '[JSON,PlainText] String

type UserAPIs = "user" :>
  ("create" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String)
   
type APIs = TodoAPIs  :<|> UserAPIs :<|> ApplicationAPIs

-- TODO
createTodo :: String -> Handler String
createTodo = return

updateTodo :: String -> Handler String
updateTodo = return

fetchAllTodo :: String -> Handler String
fetchAllTodo = return

getDetailsTodo :: String -> Handler String
getDetailsTodo = return

-- APPLICATION
appCheck :: Handler String
appCheck =  return "Todo App is Running"

-- USER

createUser :: String -> Handler String
createUser = return

updateUser :: String -> Handler String
updateUser = return
