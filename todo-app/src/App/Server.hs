module App.Server where

import qualified Routes.Routes as R
import Servant

todoServer :: Server R.APIs
todoServer =  todoServers' 

todoServers' = todoAPI :<|> userAPI :<|> applicationAPI

todoAPI = R.createTodo :<|> R.updateTodo :<|> R.fetchAllTodo :<|> R.getDetailsTodo

userAPI = R.createUser :<|> R.updateUser

applicationAPI = R.appCheck

