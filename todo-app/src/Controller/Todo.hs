module Controller.Todo where

import qualified Storage.Types.API as SA
import qualified Flow as F
import qualified Storage.KV.Queries as KVQ
import Data.Text
import Servant
import qualified Storage.DB.Queries.Todo as QT
import qualified Database.Beam.Backend as B
import Control.Monad.IO.Class
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Utils.DateTime as DateTime

createTask :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTask req@SA.CreateTodoRequest {task,description} = do
  now <- liftIO $ DateTime.getCurrentTimeIST
  id <- liftIO $ UUID.toText <$> UUID.nextRandom
  let status = "PENDING"
  -- dbInsert <- QT.createTask req
  kvInsert <- KVQ.setExKey task $ SA.CreateTodoResponse id task description status now
  -- send callback to python server for task creation
  undefined

-- updateTask

-- deleteTask
