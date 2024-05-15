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
import Data.Types.Error (singleResult)
import Control.Monad
import Data.UUID.V4 (nextRandom)
import Data.Password.Argon2
import Servant.Auth.JWT
import Text.Blaze (ToMarkup (toMarkup))
import qualified Text.Blaze.Html5 as H
import Servant (FromHttpApiData (parseQueryParam))
import Web.FormUrlEncoded
import Data.Html.Page

-- What we include in JWTs. Make it as small as possible,
-- and don't store anything that can change between requests.
newtype UserId = UserId { unUserId :: UUID }
  deriving (Eq, Ord, Show, Generic)
instance FromHttpApiData UserId where
  parseQueryParam t = UserId <$> parseQueryParam t
instance FromField UserId where
  fromField :: FieldParser UserId
  fromField f = UserId <$> fromField f
instance ToField UserId where
  toField = toField . unUserId
instance ToJWT UserId
instance FromJWT UserId
instance ToJSON UserId where
  toJSON = toJSON . unUserId
instance FromJSON UserId where
  parseJSON v = UserId <$> parseJSON v

data User = User
  { userId :: UserId
  , userName :: Text
  } deriving (Eq, Ord, Show, Generic)

displayUser :: User -> H.Html
displayUser u = H.div $ mconcat
  [ H.p $ H.toHtml $ "ID: " <> show u.userId
  , H.p $ H.toHtml $ "Name: " <> show u.userName
  ]

instance ToMarkup User where
  toMarkup = basePage . displayUser
instance ToMarkup [User] where
  toMarkup = basePage . H.ul . mconcat . fmap (H.li . displayUser)

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

data CreateUser = CreateUser
  { createUserName :: Text
  , createUserPassword :: Text
  } deriving (Eq, Ord, Generic)
instance FromJSON CreateUser where
  parseJSON = withObject "CreateUser" $ \o -> CreateUser
    <$> o .: "name"
    <*> o .: "password"

data Login = Login
  { loginUser :: Text
  , loginPass :: Text
  }
instance FromJSON Login where
  parseJSON = withObject "Login" $ \o -> Login
    <$> o .: "user"
    <*> o .: "password"

instance FromForm Login where
  fromForm f = Login
    <$> parseUnique "user" f
    <*> parseUnique "password" f

getUser :: CanAppM m c e => UserId -> m User
getUser uid = do
  c <- asks conn
  singleResult <=< liftIO $ query c "select id, name from user where id = ?" (Only uid)

addUser :: CanAppM m c e => CreateUser -> m UserId
addUser create = do
  c <- asks conn
  uid <- UserId <$> liftIO nextRandom
  liftIO $ execute c "insert into user (id, name) values (?, ?)" (uid, create.createUserName)
  hash <- hashPassword $ mkPassword create.createUserPassword
  liftIO $ execute c "insert into user_pass(id, hash) values (?, ?)" (uid, hash)
  pure uid

getUsers :: CanAppM m c e => m [User]
getUsers = do
  c <- asks conn
  liftIO $ query_ c "select id, name from user"