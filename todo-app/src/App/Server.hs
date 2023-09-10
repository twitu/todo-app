module App.Server where

import qualified Routes.Routes as R
import Servant

todoServer :: Server R.APIs
todoServer = (R.create :<|> R.update :<|> R.fetchAll :<|> R.getDetails :<|> R.delete) :<|> R.appCheck
