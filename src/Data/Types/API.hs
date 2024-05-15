module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Text.Blaze.Html
import Servant.HTML.Blaze
import Data.Types.User (User, Login, CreateUser)
import Text.Blaze ()
import Data.Text
import Data.Types.Message
import Data.Types.Auth

-- Set-Cookie is here twice, as both JWT and XSRF cookies are sent.
type SetLoginCookies a = Headers
  '[ Header "Set-Cookie" SetCookie
   , Header "Set-Cookie" SetCookie
   , Header "Location" Text
   , Header "Id" Text
   ] a

topAPI :: Proxy TopAPI
topAPI = Proxy

type TopAPI =
    AuthLogin :> MainAPI
  :<|> LoginAPI
  :<|> "static" :> Raw

type GetLogin = "login" :> Get '[HTML] Html
type PostLogin = "login" :> ReqBody '[FormUrlEncoded, JSON] Login :> Verb 'POST 303 '[HTML, JSON] (SetLoginCookies ())
type GetRegister = "register" :> Get '[HTML] Html
type PostRegister = "register" :> ReqBody '[FormUrlEncoded, JSON] CreateUser :> Verb 'POST 303 '[JSON] (SetLoginCookies ())
type LoginAPI =
  PostLogin
    :<|> PostRegister

type MainAPI = HtmlAPI :<|> CoreAPI

type GetRoot = Get '[HTML] Html
type GetMessage = "message" :> Get '[HTML] Html
type HtmlAPI =
       -- Server root, does not require auth
       GetRoot
  :<|> GetLogin
  :<|> GetRegister
       -- Form to send a new message, requires auth
  :<|> GetMessage

-- All of CoreAPI requires auth
type CoreAPI = 
       GetMessagesApi
  :<|> GetAllMessagesApi
  :<|> PostMessageApi
  :<|> GetUsersApi
-- Break out route types so they are cleaner to use when building internal links.
type GetMessagesApi    = "messages" :>                                                  Get  '[HTML, JSON] (AuthedValue [Message])
type GetAllMessagesApi = "messages" :> "all"                                         :> Get  '[HTML, JSON] (AuthedValue AllMessages)
type PostMessageApi    = "message"  :> ReqBody '[FormUrlEncoded, JSON] CreateMessage :> Post '[HTML, JSON] MessagePosted
type GetUsersApi       = "users"    :>                                                  Get  '[HTML, JSON] (AuthedValue [User])

-- | Helper function when generating typesafe internal API links.
linkText ::
  ( IsElem endpoint TopAPI,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint Link)
  ) =>
  Proxy endpoint ->
  Text
linkText api = pack "/" <> toUrlPiece (safeLink topAPI api)