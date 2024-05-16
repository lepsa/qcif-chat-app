module Data.Html.Root where

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Html.Page
import Data.Types.Auth
import Data.Html.Util

root :: Authed -> H.Html
root auth = basePage auth $ mconcat
  [ H.h3 "Welcome to the QCIF Interview Chat Server."
  , whenNotLoggedIn auth $ H.p "Please register or log in to access messaging services."
  , H.p "Messages on this service are stored in a persistent database, so feel free to message someone even if they are off line."
  , H.p "When you view messages, you will be shown all of your new messages since you last checked. If you want to view older messages you can find them by following the \"Get all messages\" link on the Messages page."
  , H.p "When viewing the \"Messages\" page, refreshing the page or clicking the \"Refresh Messages\" link will fetch any new messages from the server. It will not show messages you have previously seen."
  , H.p "Refreshing the \"All Messages\" page will fetch all of your messages, regardless of if you have seen them or not."
  ]