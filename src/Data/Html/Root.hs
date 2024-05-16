module Data.Html.Root where

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Html.Page
import Data.Types.Auth

root :: Authed -> H.Html
root auth = basePage auth $ mconcat
  [ H.p "foo bar"
  ]