module Data.Types.User where

import Data.UUID
import Data.Text
import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple.FromField
import Data.Types.Orphans ()
import Database.SQLite.Simple.ToField
import Data.Types.AppM (CanAppM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Types.Env
import Database.SQLite.Simple
import Data.Types.Error (singleResult, catchDbException)
import Control.Monad
import Data.UUID.V4 (nextRandom)
import Data.Password.Argon2
import Servant.Auth.JWT
import Servant (FromHttpApiData (parseQueryParam))
import Web.FormUrlEncoded
import Data.OpenApi
import Data.Types.Util
import Data.Data

-- What we include in JWTs. Make it as small as possible,
-- and don't store anything that can change between requests.
data UserLogin = UserLogin
  { userLoginId :: UserId
  , userLoginName :: Text
  } deriving (Eq, Ord, Show, Generic, Typeable)
instance ToJSON UserLogin where
  toJSON u = object
    [ ("id" .= u.userLoginId)
    , ("name" .= u.userLoginName)
    ]
instance FromJSON UserLogin where
  parseJSON = withObject "UserLogin" $ \o -> UserLogin
    <$> o .: "id"
    <*> o .: "name"
instance ToJWT UserLogin
instance FromJWT UserLogin
instance ToSchema UserLogin where
  declareNamedSchema = schemaOpts "userLogin"

newtype UserId = UserId { unUserId :: UUID }
  deriving (Eq, Ord, Show, Generic, Typeable)
instance FromHttpApiData UserId where
  parseQueryParam t = UserId <$> parseQueryParam t
instance FromField UserId where
  fromField :: FieldParser UserId
  fromField f = UserId <$> fromField f
instance ToField UserId where
  toField = toField . unUserId
instance ToJSON UserId where
  toJSON = toJSON . unUserId
instance FromJSON UserId where
  parseJSON v = UserId <$> parseJSON v
instance ToSchema UserId where
  declareNamedSchema _ = declareNamedSchema $ Proxy @UUID

data User = User
  { userId :: UserId
  , userName :: Text
  } deriving (Eq, Ord, Show, Generic, Typeable)

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToJSON User where
  toJSON u = object
    [ "id" .= u.userId
    , "name" .= u.userName
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> o .: "name"

instance ToSchema User where
  declareNamedSchema = schemaOpts "user"

data CreateUser = CreateUser
  { createUserUser :: Text
  , createUserPassword :: Text
  } deriving (Eq, Ord, Generic, Typeable)

instance FromForm CreateUser where
  fromForm f = CreateUser
    <$> parseUnique "user" f
    <*> parseUnique "password" f

instance FromJSON CreateUser where
  parseJSON = withObject "CreateUser" $ \o -> CreateUser
    <$> o .: "user"
    <*> o .: "password"

instance ToSchema CreateUser where
  declareNamedSchema = schemaOpts "createUser"

data Login = Login
  { loginUser :: Text
  , loginPassword :: Text
  } deriving (Generic, Typeable)
instance FromJSON Login where
  parseJSON = withObject "Login" $ \o -> Login
    <$> o .: "user"
    <*> o .: "password"

instance FromForm Login where
  fromForm f = Login
    <$> parseUnique "user" f
    <*> parseUnique "password" f

instance ToSchema Login where
  declareNamedSchema = schemaOpts "login"

getUser :: CanAppM m c e => UserId -> m User
getUser uid = do
  c <- asks conn
  singleResult <=< catchDbException $ query c "select id, name from user where id = ?" (Only uid)

addUser :: CanAppM m c e => CreateUser -> m User
addUser create = do
  c <- asks conn
  uid <- UserId <$> liftIO nextRandom
  catchDbException $ execute c "insert into user (id, name) values (?, ?)" (uid, create.createUserUser)
  hash <- hashPassword $ mkPassword create.createUserPassword
  catchDbException $ execute c "insert into user_pass(id, hash) values (?, ?)" (uid, hash)
  pure $ User uid create.createUserUser

getUsers :: CanAppM m c e => m [User]
getUsers = do
  c <- asks conn
  catchDbException $ query_ c "select id, name from user"