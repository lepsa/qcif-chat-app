module Data.Html.Root where

import Data.Types.API
import Data.Types.AppM
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Html.Page

root :: CanAppM m c e => Authed -> m H.Html
root _auth = pure $ basePage $ H.p "foo bar"