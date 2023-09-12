module App.App where

import Servant
import qualified Routes.Routes as R
import qualified App.Server as S
import Network.Wai.Handler.Warp
import Storage.Types.App 

app :: Application
app = serve S.todoProxy . S.todoServer $ Env "Testing"

runServer :: IO ()
runServer = run 8022 app
