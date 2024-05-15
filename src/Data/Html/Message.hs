module Data.Html.Message where

import Data.Types.API
import Data.Types.AppM
import qualified Text.Blaze.Html as H
import Servant.Auth
import Data.Types.User
import Servant
import qualified Text.Blaze.Html5 as H
import Text.Blaze
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Html.Page (basePage)

newMessage :: CanAppM m c e => Authed -> m H.Html
newMessage _auth = pure $ basePage $
  H.form
    ! HA.method "POST"
    ! HA.action (textValue $ linkText (Proxy @(Auth Auths UserId :> PostMessageApi)))
    $ mconcat
    [ H.label ! HA.for "to" $ "To"
    , H.input ! HA.name "to" ! HA.type_ "text"
    , H.br
    , H.label ! HA.for "body" $ "Body"
    , H.input ! HA.name "body" ! HA.type_ "text"
    , H.br
    , H.input ! HA.type_ "submit" ! HA.value "Send"
    ]