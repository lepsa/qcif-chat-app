module Data.Html.Login where

import qualified Text.Blaze.Html as H
import Data.Types.API
import Data.Data
import Data.Html.Page
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html
import Data.Types.Auth
import Servant

login :: Authed -> H.Html
login auth = basePage auth $ mconcat
  [ H.h3 "Login"
  , H.form
    ! HA.method "POST"
    ! HA.action (textValue $ linkText (Proxy @(AuthLogin :> PostLogin)))
    $ mconcat
    [ H.label ! HA.for "user" $ "Username"
    , H.input ! HA.name "user" ! HA.type_ "text"
    , H.br
    , H.label ! HA.for "password" $ "password"
    , H.input ! HA.name "password" ! HA.type_ "password"
    , H.br
    , H.input ! HA.type_ "submit" ! HA.value "Send"
    ]
  ]