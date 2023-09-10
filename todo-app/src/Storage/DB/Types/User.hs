{-# Language RecordWildCards #-}
{-# Language StandaloneDeriving #-}

module Storage.DB.Types.User where
  
import Data.Aeson
import Data.Time
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as BST
import qualified Storage.DB.Utils as DU
import Data.Text

data UsersT f =
  Users
    {
      _id :: BST.C f  Text,
      _name :: BST.C f Text,
      _email :: BST.C f (Maybe Text),
      _active :: BST.C f Bool,
      _createdAt :: BST.C f LocalTime,
      _updatedAt :: BST.C f LocalTime,
      _udf :: BST.C f (Maybe Value)
    } deriving (B.Generic, BST.Beamable)

type Users = UsersT B.Identity

type UsersPrimary = BST.PrimaryKey UsersT B.Identity

instance BST.Table UsersT where
  data PrimaryKey UsersT f = UsersPrimary (BST.C f Text)
                             deriving (B.Generic, BST.Beamable)
  primaryKey = UsersPrimary . _id

deriving instance Show Users

deriving instance Eq Users

instance FromJSON Users where
  parseJSON = genericParseJSON DU.stripAllLensPrefixOptions

instance ToJSON Users where
  toJSON =
    genericToJSON (DU.stripAllLensPrefixOptions {omitNothingFields = True})

userEMod :: 
     Text 
  -> Text 
  -> BST.EntityModification (BST.DatabaseEntity be db) be (BST.TableEntity UsersT)
userEMod tableName schema = 
  BST.setEntitySchema (Just schema) <>
  BST.setEntityName tableName <>
  BST.modifyTableFields
    BST.tableModification
      { _id = "id"
      , _name = "name"
      , _email = "emial"
      , _active = "active"
      , _createdAt = "createdAt"
      , _updatedAt = "updatedAt"
      , _udf = "udf"
      }

insertExpression c = insertExpressions [c]

insertExpressions cs = B.insertExpressions (toRowExpression <$> cs)
  where
    toRowExpression Users {..} =
      Users
        (B.val_ _id)
        (B.val_ _name)
        (B.val_ _email)
        (B.val_ _active)
        (B.val_ _createdAt)
        (B.val_ _updatedAt)
        (B.val_ _udf)


