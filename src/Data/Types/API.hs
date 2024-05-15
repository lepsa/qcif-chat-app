module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.Server
import Text.Blaze.Html
import Servant.HTML.Blaze
import Data.Types.User (UserId, User, Login, Register, CreateUser)
import Data.Types.User qualified as U
import Data.Types.Message
import Data.Types.AppM (AppM, runAppM)
import Data.Types.Env
import Data.Types.Error
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Types.Auth
import Control.Monad.Trans.Except
import Control.Monad

-- All of the auth types we want to support.
-- Any of these can be used on any route.
type Auths = '[BasicAuth, Cookie, JWT]
type AuthLogin = Auth Auths UserId
type Authed = AuthResult UserId

-- Set-Cookie is here twice, as both JWT and XSRF cookies are sent.
type SetLoginCookies a = Headers
  '[ Header "Set-Cookie" SetCookie
   , Header "Set-Cookie" SetCookie
   ] a

topAPI :: Proxy TopAPI
topAPI = Proxy

type TopAPI =
    AuthLogin:> MainAPI
  :<|> LoginAPI

type LoginAPI =
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] (SetLoginCookies ())
    :<|> "register" :> ReqBody '[JSON] CreateUser :> Post '[JSON] (SetLoginCookies ())

type MainAPI = HtmlAPI :<|> JsonAPI

type HtmlAPI = Get '[HTML] Html
type JsonAPI = 
       "messages" :> Get '[JSON] [Message]
  :<|> "messages" :> "all" :> Get '[JSON] [Message]
  :<|> "message" :> ReqBody '[JSON] CreateMessage :> Post '[JSON] ()
  :<|> "users" :> Get '[JSON] [User]

server :: CookieSettings -> JWTSettings -> ServerT TopAPI (AppM IO Env AppError)
server cookieSettings jwtSettings =
    mainServer
  :<|> login
  :<|> register
  where
    login l = do
      c <- asks conn
      uid <- either throwError_ pure <=< runExceptT $ checkUserPassword c l.loginUser l.loginPass
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings uid
      case mApplyCookies of
        Nothing -> throwError_ $ Other "Could not apply login cookies"
        Just cookies -> pure $ cookies ()
    register createUser = do
      uid <- U.addUser createUser
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings uid
      case mApplyCookies of
        Nothing -> throwError_ $ Other "Could not apply login cookies"
        Just cookies -> pure $ cookies ()

mainServer :: Authed -> ServerT MainAPI (AppM IO Env AppError)
mainServer auth = htmlServer auth :<|> jsonServer auth

htmlServer :: Authed -> ServerT HtmlAPI (AppM IO Env AppError)
htmlServer _auth = pure mempty

jsonServer :: Authed -> ServerT JsonAPI (AppM IO Env AppError)
jsonServer (Authenticated uid) =
       getMessages
  :<|> getAllMessages
  :<|> postMessage
  :<|> getUsers
  where
    getMessages = do
      t <- liftIO getCurrentTime
      getSyncedMessages t uid
    getAllMessages = getAllMessagesForUser uid
    postMessage msg = void $ writeMessage uid msg
    getUsers = U.getUsers
jsonServer _ = hoistServer (Proxy @JsonAPI) serverNat $ throwAll err401

serverNat :: AsError e' e => AppM IO Env e a -> AppM IO Env e' a
serverNat n = do
  c <- ask
  e <- liftIO $ runAppM c n
  either throwError_ pure e