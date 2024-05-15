module Data.Html.Register where

import Data.Types.API
import qualified Text.Blaze.Html5 as H
import Data.Types.AppM
import Data.Html.Page
import Text.Blaze.Html
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Data

register :: CanAppM m c e => Authed -> m H.Html
register _auth = pure $ basePage $ H.form
  ! HA.method "POST"
  ! HA.action (textValue $ linkText (Proxy @PostRegister))
  $ mconcat
  [ H.label ! HA.for "user" $ "Username"
  , H.input ! HA.name "user" ! HA.type_ "text"
  , H.br
  , H.label ! HA.for "password" $ "password"
  , H.input ! HA.name "password" ! HA.type_ "password"
  , H.br
  , H.input ! HA.type_ "submit" ! HA.value "Send"
  ]
