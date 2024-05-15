module Data.Html.Page where
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import Data.Text
import Text.Blaze.Html
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Types.API
import Servant
import Data.Html.Util
import Data.Types.Auth

basePage :: Authed -> H.Html -> H.Html
basePage auth content = H.html $ mconcat
  [ pageHead
  , pageContent auth content
  ]

pageContent :: Authed -> H.Html -> H.Html
pageContent auth content = H.body $ mconcat
  [ contentHeader
  , H.main $ mconcat
    [ sideNav auth
    , H.div ! HA.id "content" $ content
    ]
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
contentHeader = H.header
  $ H.h1 $ H.a ! HA.href "/" $ "Chat App"

contentFooter :: H.Html
contentFooter = H.footer mempty

sideNav :: Authed -> H.Html
sideNav auth = H.nav $ mconcat
  [ H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetLogin)) $ "Login"
  , H.br
  , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetRegister)) $ "Register"
  , whenLoggedIn auth $ \_userId -> mconcat
    [ H.br
    , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetMessagesApi)) $ "Messages"
    , H.br
    , H.a ! HA.href (H.textValue $ linkText $ Proxy @(AuthLogin :> GetMessage)) $ "New Message"
    ]]