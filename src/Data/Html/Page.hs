module Data.Html.Page where
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Text
import Text.Blaze.Html
import qualified Text.Blaze.Html5.Attributes as HA

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

siteTitle :: Text
siteTitle = "QCRF Chat App"

pageHead :: H.Html
pageHead = H.head $ mconcat
  [ H.link ! HA.rel (H.stringValue "stylesheet") ! HA.href (stringValue "/static/main.css"),
    H.script ! HA.src (H.stringValue "/static/main.js") $ mempty,
    H.script ! HA.src (H.stringValue "/static/htmx.min.js") $ mempty,
    H.title $ H.toHtml siteTitle
  ]

contentHeader :: H.Html
contentHeader = H.header $ H.h1 "Chat App"

contentFooter :: H.Html
contentFooter = H.footer mempty