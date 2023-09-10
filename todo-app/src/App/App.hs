module App.App where

import Servant
import qualified Routes.Routes as R
import qualified App.Server as S
import Network.Wai.Handler.Warp

todoProxy :: Proxy R.APIs
todoProxy = Proxy
 
app :: Application
app = serve todoProxy S.todoServer

runServer :: IO ()
runServer = run 8083 app
