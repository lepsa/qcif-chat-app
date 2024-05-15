{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Html.User where

import Text.Blaze
import Data.Types.User
import qualified Text.Blaze.Html as H
import Data.Html.Page
import qualified Text.Blaze.Html5 as H
import Data.Types.Auth

instance ToMarkup (AuthedValue User) where
  toMarkup a = basePage a.auth $ displayUser a.value
instance ToMarkup (AuthedValue [User]) where
  toMarkup a = basePage a.auth $ H.ul . mconcat $ H.li . displayUser <$> a.value

displayUser :: User -> H.Html
displayUser u = H.div $ mconcat
  [ H.p $ H.toHtml $ "ID: " <> show u.userId
  , H.p $ H.toHtml $ "Name: " <> show u.userName
  ]
