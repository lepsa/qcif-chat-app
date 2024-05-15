module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.Server
import Text.Blaze.Html
import Servant.HTML.Blaze
import Data.Types.User (UserId, User, Login, CreateUser)
import Data.Types.Message
import Text.Blaze ()
import Data.Text

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
  "login" :> Get '[HTML] Html
    :<|> "login" :> ReqBody '[FormUrlEncoded, JSON] Login :> Post '[HTML, JSON] (SetLoginCookies ())
    :<|> "register" :> ReqBody '[JSON] CreateUser :> Post '[JSON] (SetLoginCookies ())

type MainAPI = HtmlAPI :<|> CoreAPI

type HtmlAPI =
  -- Server root, does not require auth
    Get '[HTML] Html
  :<|> "message" :> Get '[HTML] Html
type CoreAPI = 
       GetMessagesApi
  :<|> GetAllMessagesApi
  :<|> PostMessageApi
  :<|> GetUsersApi
-- Break out route types so they are cleaner to use when building internal links.
type GetMessagesApi    = "messages" :>                                                  Get  '[HTML, JSON] [Message]
type GetAllMessagesApi = "messages" :> "all"                                         :> Get  '[HTML, JSON] [Message]
type PostMessageApi    = "message"  :> ReqBody '[FormUrlEncoded, JSON] CreateMessage :> Post '[HTML, JSON] ()
type GetUsersApi       = "users"    :>                                                  Get  '[HTML, JSON] [User]

-- | Helper function when generating typesafe internal API links.
linkText ::
  ( IsElem endpoint TopAPI,
    HasLink endpoint,
    ToHttpApiData (MkLink endpoint Link)
  ) =>
  Proxy endpoint ->
  Text
linkText api = pack "/" <> toUrlPiece (safeLink topAPI api)