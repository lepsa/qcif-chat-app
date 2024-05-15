{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Html.Util where

import Data.Types.User
import qualified Text.Blaze.Html5 as H
import Servant.Auth.Server
import Text.Blaze
import Data.Types.Auth

whenLoggedIn :: Authed -> (UserLogin -> H.Html) -> H.Html
whenLoggedIn (Authenticated u) f = f u
whenLoggedIn _ _ = mempty

hxBoost :: H.Attribute
hxBoost = customAttribute "hx-boost" "true"

hxOn :: String -> AttributeValue -> H.Attribute
hxOn event = customAttribute (H.stringTag $ "hx-on" <> event)

hxTrigger :: AttributeValue -> H.Attribute
hxTrigger = customAttribute "hx-trigger"

hxSwap :: AttributeValue -> H.Attribute
hxSwap = customAttribute "hx-swap"

hxTarget :: AttributeValue -> H.Attribute
hxTarget = customAttribute "hx-target"

hxGet :: AttributeValue -> H.Attribute
hxGet = customAttribute "hx-get"

hxDelete :: AttributeValue -> H.Attribute
hxDelete = customAttribute "hx-delete"

hxPut :: AttributeValue -> H.Attribute
hxPut = customAttribute "hx-put"

hxPost :: AttributeValue -> H.Attribute
hxPost = customAttribute "hx-post"

hxIndicator :: AttributeValue -> H.Attribute
hxIndicator = customAttribute "hx-indicator"

hxConfirm :: AttributeValue -> H.Attribute
hxConfirm = customAttribute "hx-confirm"

instance ToMarkup () where
  toMarkup _ = mempty