{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Html.Error where
import Servant.Server
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import Data.Html.Page
import Servant.Auth.Server
import qualified Text.Blaze.Html5 as H
import Data.Types.Error

internalError :: ServerError
internalError = err500
  { errBody = H.renderHtml $ basePage Indefinite $
    H.p "Internal Server Error"
  }

unauthorised :: ServerError
unauthorised = err401
  { errBody = H.renderHtml $ basePage Indefinite $
    H.p "Unauthorised"
  }

instance AsError ServerError AppError where
  fromError (Servant e) = e
    { errBody = H.renderHtml $ basePage Indefinite $ H.p $ H.toHtml e.errReasonPhrase
    }
  fromError BadAuth = unauthorised
  fromError (DB _) = internalError
  toError = pure . Servant