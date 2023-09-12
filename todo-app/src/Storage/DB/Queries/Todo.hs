module Storage.DB.Queries.Todo where

import qualified Database.Beam.Postgres as BP
import qualified Storage.DB.Types.DB as DB
import qualified Database.Beam as B
import qualified Storage.DB.Types.Todo as Todo
import qualified Storage.Types.API as SA
import qualified Storage.DB.Types.Todo as TT
import Data.Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Time as DT
import qualified Utils.DateTime as DateTime
import qualified Storage.DB.DBConfig as DBConf
import Control.Monad.IO.Class
import qualified Flow as F
import qualified Database.Beam.Backend as B

todoTable :: Text -> B.DatabaseEntity be DB.TodoDB (B.TableEntity Todo.TodosT)
todoTable  = DB.todo . DB.todoDB "Todos"

-- createTask ::(B.BeamSqlBackend be, B.MonadBeam be F.Flow) => Text -> SA.CreateTodoRequest -> F.Flow ()
-- createTask req@SA.CreateTodoRequest {task,description} = do
--   conn <- liftIO $ DBConf.dbGetConnection
--   id <- liftIO $ UUID.toText <$> UUID.nextRandom
--   now <- liftIO $ DateTime.getCurrentTimeIST
--   B.runInsert $ B.insert (todoTable "public") $ B.insertValues [ TT.Todos id task description "PENDING" True now now Nothing]
--   return ()



-- updateTask tableName value = do

-- findTask tableName value = do

-- findAllTask tableName value= do