module Data.Html.Root where

import Data.Types.AppM
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Html.Page
import Data.Types.Auth

root :: CanAppM m c e => Authed -> m H.Html
root auth = pure $ basePage auth $ mconcat
  [ H.p "foo bar"
  ]