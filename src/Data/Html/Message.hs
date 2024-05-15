{-# OPTIONS_GHC -Wno-orphans #-}
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
import Data.Html.Util
import Data.Types.Message
import Data.Types.Auth

mkOption :: User -> H.Html
mkOption u = H.option
  ! HA.value (stringValue . show $ unUserId u.userId)
  $ H.toHtml u.userName

newMessage :: CanAppM m c e => Authed -> m H.Html
newMessage auth@(Authenticated userId) = do
  users <- filter (\u -> u.userId /= userId) <$> getUsers
  pure $ basePage auth $
    H.form
      ! HA.method "POST"
      ! HA.action (textValue $ linkText (Proxy @(Auth Auths UserId :> PostMessageApi)))
      ! hxBoost
      ! hxOn "::config-request" "setXsrfHeader(event)"
      ! hxTarget "this"
      ! hxSwap "outerHTML"
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

displayMessage :: Message -> H.Html
displayMessage m = H.div $ H.p $ mconcat
    [ H.toHtml $ "ID: " <> show m.messageId
    , H.br
    , H.toHtml $ "From: " <> show m.messageFrom
    , H.br
    , H.toHtml $ "To: " <> show m.messageTo
    , H.br
    , H.toHtml $ "Sent: " <> show m.messageSent
    , H.br
    , H.toHtml $ "Body: " <> show m.messageBody
    ]

displayMessages :: Bool -> [Message] -> H.Html
displayMessages allMsgs messages = mconcat
    [ H.h3 "Messages"
    , if null messages
      then H.p "No new messages"
      else H.ul . mconcat $ (H.li . displayMessage) <$> messages
    , if allMsgs
      then mempty
      else H.p $ H.a ! HA.href (textValue $ linkText $ Proxy @(AuthLogin :> GetAllMessagesApi)) $ "Get all messages"
    ]

instance ToMarkup (AuthedValue AllMessages) where
  toMarkup a = basePage a.auth $ displayMessages True a.value.allMessages

instance ToMarkup (AuthedValue [Message]) where
  toMarkup a = basePage a.auth $ displayMessages False a.value

instance ToMarkup MessagePosted where
  toMarkup _ = H.p "Message sent"