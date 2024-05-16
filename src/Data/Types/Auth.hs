{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Types.Auth where

import Database.SQLite.Simple
import Servant.Auth.Server
import Data.Types.User
import Control.Monad
import Control.Monad.Except
import Data.Text.Encoding
import Control.Monad.IO.Class
import Data.Password.Argon2
import Data.Types.Error
import Data.Text
import Data.Aeson

newtype BasicAuthCfg' = BasicAuthCfg' Connection
type instance BasicAuthCfg = BasicAuthCfg'

instance FromBasicAuthData UserLogin where
  fromBasicAuthData (BasicAuthData user pass) (BasicAuthCfg' conn) = do
    either pure (pure . Authenticated) <=< runExceptT $ do
      -- Decode the user into a friendlier type
      name <- either (const $ throwError BadPassword) pure (decodeUtf8' user)
      -- Convert the type for the hash
      password <- either (const $ throwError BadPassword) pure (decodeUtf8' pass)
      checkUserPassword conn name password

checkUserPassword :: MonadIO m => Connection -> Text -> Text -> ExceptT (AuthResult UserLogin) m UserLogin
checkUserPassword c name pass = do
  u :: User <- singleResult <=< catchDbException
    $ query c "select id, name from user where name = ?" (Only name)
  hash <- singleResult <=< catchDbException $ query c "select hash from user_pass where id = ?" (Only u.userId)
  case checkPassword (mkPassword pass) hash of
    PasswordCheckFail -> throwError BadPassword
    PasswordCheckSuccess -> pure $ UserLogin u.userId u.userName

-- All of the auth types we want to support.
-- Any of these can be used on any route.
type Auths = '[BasicAuth, Cookie, JWT]
type AuthLogin = Auth Auths UserLogin
type Authed = AuthResult UserLogin

data AuthedValue a = AuthedValue
  { auth :: Authed
  , value :: a
  }
instance ToJSON a => ToJSON (AuthedValue a) where
  toJSON = toJSON . value