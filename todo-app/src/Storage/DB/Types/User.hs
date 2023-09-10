module Storage.DB.Types.User where
  
import Data.Aeson
import Data.Time
import qualified Database.Beam as BM
import qualified Database.Beam.Schema.Tables as B
import qualified Storage.DB.Utils as DU
import Data.Text

data UsersT f =
  Users
    {
      _id :: B.C f  Text,
      _name :: B.C f Text,
      _email :: B.C f (Maybe Text),
      _active :: B.C f Bool,
      _createdAt :: B.C f LocalTime,
      _updatedAt :: B.C f LocalTime,
      _udf :: B.C f (Maybe Value)
    } deriving (BM.Generic, B.Beamable)

type Users = UsersT BM.Identity

type UsersPrimary = B.PrimaryKey UsersT BM.Identity

instance B.Table UsersT where
  data PrimaryKey UsersT f = UsersPrimary (B.C f Text)
                             deriving (BM.Generic, B.Beamable)
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
  -> B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity UsersT)
userEMod tableName schema = 
  B.setEntitySchema (Just schema) <>
  B.setEntityName tableName <>
  B.modifyTableFields
    B.tableModification
      { _id = "id"
      , _name = "name"
      , _email = "email"
      , _active = "active"
      , _createdAt = "createdAt"
      , _updatedAt = "updatedAt"
      , _udf = "udf"
      }

insertExpression c = insertExpressions [c]

insertExpressions cs = BM.insertExpressions (toRowExpression <$> cs)
  where
    toRowExpression Users {..} =
      Users
        (BM.val_ _id)
        (BM.val_ _name)
        (BM.val_ _email)
        (BM.val_ _active)
        (BM.val_ _createdAt)
        (BM.val_ _updatedAt)
        (BM.val_ _udf)


