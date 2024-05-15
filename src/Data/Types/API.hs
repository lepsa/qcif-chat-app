module Data.Types.API where

import Servant hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.Server
import Data.UUID
import Data.Text
import Text.Blaze.Html
import Servant.HTML.Blaze

-- All of the auth types we want to support.
-- Any of these can be used on any route.
type Auths = '[BasicAuth, Cookie, JWT]
-- Set-Cookie is here twice, as both JWT and XSRF cookies are sent.
type SetLoginCookies a = Headers
  '[ Header "Set-Cookie" SetCookie
   , Header "Set-Cookie" SetCookie
   , Header "Location" Text
   ] a

-- What we include in JWTs. Make it as small as possible,
-- and don't store anything that can change between requests.
newtype UserKey = UserKey { unUserKey :: UUID }

type TopAPI = Auth Auths UserKey :> API

type API = HtmlAPI :<|> JsonAPI

type HtmlAPI = Get '[HTML] Html
type JsonAPI = Get '[JSON] ()

