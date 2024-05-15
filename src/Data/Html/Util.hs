module Data.Html.Util where

import Data.Types.API
import Data.Types.User
import qualified Text.Blaze.Html5 as H
import Servant.Auth.Server

whenLoggedIn :: Authed -> (UserId -> H.Html) -> H.Html
whenLoggedIn (Authenticated userId) f = f userId
whenLoggedIn _ _ = mempty