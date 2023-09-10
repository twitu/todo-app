module Storage.DB.Types.DB where

import qualified Database.Beam as B
import qualified Storage.DB.Types.Todo as Todo
import qualified Storage.DB.Types.User as User
import Data.Text

data TodoDB f =
  TodoDB 
    {
      user :: f (B.TableEntity User.UsersT) ,
      todo :: f (B.TableEntity Todo.TodosT)
    } deriving (B.Generic, B.Database be)

todoDB :: Text -> Text -> B.DatabaseSettings be TodoDB
todoDB table schema = 
  B.defaultDbSettings `B.withDbModification`
  B.dbModification
    {
      user = User.userEMod table schema,
      todo = Todo.todoEMod table schema
    }