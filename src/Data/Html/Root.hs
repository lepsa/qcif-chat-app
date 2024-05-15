module Data.Html.Root where

import Data.Types.API
import Data.Types.AppM
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H

root :: CanAppM m c e => Authed -> m H.Html
root _auth = pure $ H.p "foo bar"