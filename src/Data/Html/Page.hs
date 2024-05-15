module Data.Html.Page where
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H

basePage :: H.Html -> H.Html
basePage content = H.html $ mconcat
  [ pageHead
  , pageContent content
  ]

pageContent :: H.Html -> H.Html
pageContent content = H.body $ mconcat
  [ contentHeader
  , content
  , contentFooter
  ]

pageHead :: H.Html
pageHead = H.head mempty

contentHeader :: H.Html
contentHeader = H.header $ H.h1 "Chat App"

contentFooter :: H.Html
contentFooter = H.footer mempty