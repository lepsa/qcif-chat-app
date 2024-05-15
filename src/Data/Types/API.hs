module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.Server
import Data.Text
import Text.Blaze.Html
import Servant.HTML.Blaze
import Data.Types.User

-- All of the auth types we want to support.
-- Any of these can be used on any route.
type Auths = '[BasicAuth, Cookie, JWT]
-- Set-Cookie is here twice, as both JWT and XSRF cookies are sent.
type SetLoginCookies a = Headers
  '[ Header "Set-Cookie" SetCookie
   , Header "Set-Cookie" SetCookie
   , Header "Location" Text
   ] a

type TopAPI = Auth Auths UserId :> API

type API = HtmlAPI :<|> JsonAPI

type HtmlAPI = Get '[HTML] Html
type JsonAPI = Get '[JSON] ()

