module Test.Commands where

import Test.Types
import Data.Map qualified as M
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Lens
import qualified Network.HTTP.Client as H
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import Data.List
import Network.HTTP.Types
import Hedgehog.Internal.State
import Data.Aeson
import Data.Char (isPrint)
import qualified Data.ByteString as BS
import Text.Read (readMaybe)
import Data.Types.User (UserId(UserId))
import Data.UUID

genChar :: MonadGen m => m Char
genChar = Gen.filterT isPrint Gen.ascii

textRange :: Range Int
textRange = Range.linear 1 10

-- Filter out ':' so that the username can be used in Basic Auth
genName :: MonadGen m => m String
genName = Gen.string textRange $ Gen.filterT (/= ':') genChar

genPassword :: MonadGen m => m String
genPassword = Gen.string textRange genChar

extractId :: MonadFail m => H.Response body -> m UUID
extractId = maybe
  (fail "No Id header returned")
  (maybe (fail "Could not decode UUID") pure . readMaybe . BS8.unpack . snd)
  . find (\(h, _) -> h == "Id")
  . H.responseHeaders

extractJwt :: (MonadFail m, MonadTest m) => H.Response body -> m String
extractJwt = maybe
  (fail "Could not extract JWT value")
  (pure . takeWhile (/= ';') . drop 1 . dropWhile (/= '=') . BS8.unpack . snd)
  . find (BS8.isPrefixOf "JWT" . snd)
  . findSetCookies
  . H.responseHeaders      

findSetCookies :: [Header] -> [Header]
findSetCookies = filter (\(k, v) -> k == "Set-Cookie" && not (BS.null v))

mkReq :: Method -> Header -> [Header] -> H.Request -> H.Request
mkReq method accept headers req = req
  { H.method = method
  , H.redirectCount = 0
  , H.requestHeaders = accept : headers <> H.requestHeaders req
  }

mkJsonReq :: Method -> [Header] -> H.Request -> H.Request
mkJsonReq method = mkReq method ("Accept", "application/json")

mkHtmlReq :: Method -> [Header] -> H.Request -> H.Request
mkHtmlReq method = mkReq method ("Accept", "application/html")

registerUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m TestState
registerUser env = Command gen exe
  [ Update $ \state input output -> state &
    users %~ M.insert output (TestUser (input ^. ruName) (input ^. ruPass) Nothing)
  ]
  where
    gen :: TestState v -> Maybe (gen (RegisterUser v))
    gen _state = Just $ RegisterUser
      <$> genName
      <*> genPassword
    exe :: RegisterUser Concrete -> m UserId
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/register"
      let req' = mkJsonReq methodPost [("Content-Type", "application/json")] req
            { H.requestBody = H.RequestBodyLBS $ encode input
            }
      res <- liftIO $ H.httpNoBody req' $ env ^. manager
      res.responseStatus === status303
      UserId <$> extractId res

loginUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m TestState
loginUser env = Command gen exe
  [ Update $ \state input output -> state &
    users %~ M.update (\u -> pure $ u & tuAuth .~ Just output) (input ^. luId)
  ]
  where
    gen :: TestState v -> Maybe (gen (LoginUser v))
    gen state = if M.null $ state ^. users 
      then Nothing
      else Just $ do
        (uid, u) <- Gen.element $ M.toList $ state ^. users
        pure $ LoginUser uid (u ^. tuName) (u ^. tuPass)
    exe :: LoginUser Concrete -> m String
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/login"
      let req' = mkJsonReq methodPost [("Content-Type", "application/json")] req
            { H.requestBody = H.RequestBodyLBS $ encode input
            }
      res <- liftIO $ H.httpNoBody req' $ env ^. manager
      res.responseStatus === status303
      extractJwt res
