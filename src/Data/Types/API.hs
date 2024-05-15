module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.Server
import Data.Text
import Text.Blaze.Html
import Servant.HTML.Blaze
import Data.Types.User (UserId, User)
import Data.Types.User qualified as U
import Data.Types.Message
import Data.Types.AppM (AppM)
import Data.Types.Env
import Data.Types.Error
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class

-- All of the auth types we want to support.
-- Any of these can be used on any route.
type Auths = '[BasicAuth, Cookie, JWT]
-- Set-Cookie is here twice, as both JWT and XSRF cookies are sent.
type SetLoginCookies a = Headers
  '[ Header "Set-Cookie" SetCookie
   , Header "Set-Cookie" SetCookie
   , Header "Location" Text
   ] a

type TopAPI = Auth Auths UserId :> MainAPI

type MainAPI = HtmlAPI :<|> JsonAPI

type HtmlAPI = Get '[HTML] Html
type JsonAPI = 
       "messages" :> Get '[JSON] [Message]
  :<|> "messages" :> "all" :> Get '[JSON] [Message]
  :<|> "message" :> ReqBody '[JSON] CreateMessage :> PostNoContent
  :<|> "users" :> Get '[JSON] [User]

server :: UserId -> ServerT MainAPI (AppM IO Env AppError)
server uid = htmlServer uid :<|> jsonServer uid

htmlServer :: UserId -> ServerT HtmlAPI (AppM IO Env AppError)
htmlServer _uid = pure mempty

jsonServer :: UserId -> ServerT JsonAPI (AppM IO Env AppError)
jsonServer uid =
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