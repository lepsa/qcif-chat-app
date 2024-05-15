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
import Servant.Auth.Server (AuthResult(Authenticated))
import Data.Types.Error

mkOption :: User -> H.Html
mkOption u = H.option
  ! HA.value (stringValue . show $ unUserId u.userId)
  $ H.toHtml u.userName

newMessage :: CanAppM m c e => Authed -> m H.Html
newMessage (Authenticated userId) = do
  users <- filter (\u -> u.userId /= userId) <$> getUsers
  pure $ basePage $
    H.form
      ! HA.method "POST"
      ! HA.action (textValue $ linkText (Proxy @(Auth Auths UserId :> PostMessageApi)))
      $ mconcat
      [ H.label ! HA.for "to" $ "To"
      , H.select ! HA.name "to" $ mconcat $
        mkOption <$> users
      , H.br
      , H.label ! HA.for "body" $ "Body"
      , H.input ! HA.name "body" ! HA.type_ "text"
      , H.br
      , H.input ! HA.type_ "submit" ! HA.value "Send"
      ]
newMessage _ = throwError_ BadAuth