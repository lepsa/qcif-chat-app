module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.Server
import Text.Blaze.Html
import Servant.HTML.Blaze
import Data.Types.User (UserId, User, Login)
import Data.Types.User qualified as U
import Data.Types.Message
import Data.Types.AppM (AppM, runAppM)
import Data.Types.Env
import Data.Types.Error
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class
import Control.Monad.Reader

-- All of the auth types we want to support.
-- Any of these can be used on any route.
type Auths = '[BasicAuth, Cookie, JWT]
type AuthLogin = Auth Auths UserId
type Authed = AuthResult UserId

-- Set-Cookie is here twice, as both JWT and XSRF cookies are sent.
type SetLoginCookies a = Headers
  '[ Header "Set-Cookie" SetCookie
   , Header "Set-Cookie" SetCookie
   -- , Header "Location" Text
   ] a

type TopAPI =
    AuthLogin:> MainAPI
  :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] (SetLoginCookies ())

type MainAPI = HtmlAPI :<|> JsonAPI

type HtmlAPI = Get '[HTML] Html
type JsonAPI = 
       "messages" :> Get '[JSON] [Message]
  :<|> "messages" :> "all" :> Get '[JSON] [Message]
  :<|> "message" :> ReqBody '[JSON] CreateMessage :> PostNoContent
  :<|> "users" :> Get '[JSON] [User]

server :: ServerT TopAPI (AppM IO Env AppError)
server = mainServer :<|> login
  where
    login = undefined

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
    postMessage msg = NoContent <$ writeMessage uid msg
    getUsers = U.getUsers
jsonServer _ = hoistServer (Proxy @JsonAPI) nat $ throwAll err401
  where
    nat :: AppM IO Env ServerError a -> AppM IO Env AppError a
    nat n = do
      c <- ask
      e <- liftIO $ runAppM c n
      either throwError_ pure e