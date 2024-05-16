{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Html.Message where

import Data.Types.API
import qualified Text.Blaze.Html as H
import Data.Types.User
import Servant
import qualified Text.Blaze.Html5 as H
import Text.Blaze
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Html.Page (basePage)
import Data.Html.Util
import Data.Types.Message
import Data.Types.Auth
import Data.List
import Data.Bool

mkOption :: User -> H.Html
mkOption u = H.option
  ! HA.value (stringValue . show $ unUserId u.userId)
  $ H.toHtml u.userName

newMessage :: Authed -> UserLogin -> [User] -> H.Html
newMessage auth user users = basePage auth $ mconcat
  [ H.h3 "New Message"
  , H.form
    ! HA.method "POST"
    ! HA.action (textValue $ linkText (Proxy @(AuthLogin :> PostMessageApi)))
    ! hxBoost
    ! hxOn "::config-request" "setXsrfHeader(event)"
    ! hxTarget "this"
    ! hxSwap "outerHTML"
    $ mconcat
    [ H.label ! HA.for "to" $ "To"
    , H.select ! HA.name "to" $ mconcat $
      mkOption <$> sortBy (\a b -> compare a.userName b.userName) filteredUsers
    , H.br
    , H.label ! HA.for "body" $ "Body"
    , H.input ! HA.name "body" ! HA.type_ "text"
    , H.br
    , H.input ! HA.type_ "submit" ! HA.value "Send"
    ]
  ]
  where
    filteredUsers = filter (\u -> u.userId /= user.userLoginId) users

displayMessage :: Message -> H.Html
displayMessage m = H.div $ H.p $ mconcat
    [ H.toHtml $ "From: " <> m.messageFromName
    , H.br
    , H.toHtml $ "To: " <> m.messageToName
    , H.br
    , H.toHtml $ "Sent: " <> show m.messageSent
    , H.br
    , H.toHtml $ "Body: " <> m.messageBody
    ]

displayMessages :: Bool -> [Message] -> H.Html
displayMessages allMsgs messages = mconcat
    [ H.h3 $ bool "Messages" "All Messages" allMsgs
    , refresh
    , if allMsgs
      then mempty
      else H.p $ H.a ! HA.href (textValue $ linkText $ Proxy @(AuthLogin :> GetAllMessagesApi)) $ "Get all messages"
    , if null messages
      then H.p "No new messages"
      else H.ul . mconcat $ (H.li . displayMessage) <$> messages
    ]
  where
    refreshLink = if allMsgs
      then textValue $ linkText $ Proxy @(AuthLogin :> GetAllMessagesApi)
      else textValue $ linkText $ Proxy @(AuthLogin :> GetMessagesApi)
    refresh = H.p $ H.a
      ! HA.href refreshLink 
      $ "Refresh Messages"

instance ToMarkup (AuthedValue AllMessages) where
  toMarkup a = basePage a.auth $ displayMessages True a.value.allMessages

instance ToMarkup (AuthedValue [Message]) where
  toMarkup a = basePage a.auth $ displayMessages False a.value

instance ToMarkup MessagePosted where
  toMarkup _ = H.p "Message sent"