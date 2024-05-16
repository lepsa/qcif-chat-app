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
  ]