module Data.Html.Root where

import Data.Types.API
import Data.Types.AppM
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Html.Page
import Data.Data
import Text.Blaze.Html
import qualified Text.Blaze.Html5.Attributes as HA
import Servant
import Data.Html.Util (whenLoggedIn)

root :: CanAppM m c e => Authed -> m H.Html
root auth = pure $ basePage $ mconcat
  [ H.p "foo bar"
  , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetLogin)) $ "Login"
  , H.br
  , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetRegister)) $ "Register"
  , whenLoggedIn auth $ \_userId -> mconcat
    [ H.br
    , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetMessagesApi)) $ "Messages"
    , H.br
    , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetMessage)) $ "New Message"
    ]
  ]