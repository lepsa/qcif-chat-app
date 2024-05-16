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
import Servant.OpenApi
import qualified Servant.Auth
import Servant hiding (BasicAuth, BadPassword)
import qualified Data.Text as T
import Data.OpenApi (SecurityScheme(..), OpenApi (_openApiComponents), _componentsSecuritySchemes, SecurityDefinitions (..), allOperations, security, SecurityRequirement (..), SecuritySchemeType (..), HttpSchemeType (..), ToSchema (declareNamedSchema))
import Control.Lens
import qualified Data.HashMap.Strict.InsOrd as HM
import Data.Data
import GHC.Generics

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
  } deriving (Generic, Typeable)
instance ToJSON a => ToJSON (AuthedValue a) where
  toJSON = toJSON . value
instance ToSchema a => ToSchema (AuthedValue a) where
  declareNamedSchema _ = declareNamedSchema $ Proxy @a

-- servant-openapi3 doesn't have default instances for auth yet.
-- https://github.com/biocad/servant-openapi3/issues/42
-- Copied from https://github.com/jumper149/mensam/blob/7ad040926cc388456292a33073dc44bd37a61201/server/source/library/Servant/Auth/OrphanInstances.hs
instance (HasOpenApi api) => HasOpenApi (Servant.Auth.Auth '[] a :> api) where
  toOpenApi Proxy = toOpenApi $ Proxy @api

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.BasicAuth : auths) a :> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "BasicAuth"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
        , _securitySchemeDescription = Just "Basic Authentication"
        }

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.JWT : auths) a :> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "JWT"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Bearer Authentication"
        }

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.Cookie : auths) a :> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "Cookie"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Cookie Authentication"
        }

addSecurityScheme :: T.Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
  openApi
    { _openApiComponents =
        (_openApiComponents openApi)
          { _componentsSecuritySchemes =
              _componentsSecuritySchemes (_openApiComponents openApi)
                <> SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
          }
    }

addSecurityRequirement :: T.Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
  allOperations
    . security
    %~ ((SecurityRequirement $ HM.singleton securityRequirement []) :)