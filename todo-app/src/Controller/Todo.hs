module Controller.Todo where

import qualified Storage.Types.API as SA
import qualified Flow as F
import qualified Storage.KV.Queries as KVQ
import Data.Text
import Servant
import qualified Storage.DB.Queries.Todo as QT
import qualified Database.Beam.Backend as B


createTask :: SA.CreateTodoRequest -> F.Flow (SA.CreateTodoResponse)
createTask req = do
  -- dbInsert <- QT.createTask req
  kvInsert <- KVQ.setExKey "abcxyz" ("aditya" :: Text)
  undefined

-- updateTask

-- deleteTask
