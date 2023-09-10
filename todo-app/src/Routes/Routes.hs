{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Routes where

import Servant

type APIs = "todo" :>
  ("create" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "update" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "fetchAll" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "getDetails" :> ReqBody '[JSON] String :> Post '[JSON] String
    :<|> "delete" :> ReqBody '[JSON] String :> Post '[JSON] String)
  :<|> ApplicationAPIs
 
type ApplicationAPIs = "application" :> "app" :> Get '[JSON,PlainText] String

create :: String -> Handler String
create = return

update :: String -> Handler String
update = return

fetchAll :: String -> Handler String
fetchAll = return

getDetails :: String -> Handler String
getDetails = return

delete :: String -> Handler String
delete = return

appCheck :: Handler String
appCheck =  return "Todo App is Running"
