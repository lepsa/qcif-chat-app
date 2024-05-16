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
import Data.Aeson
import Data.Char (isPrint)
import qualified Data.ByteString as BS
import Text.Read (readMaybe)
import Data.Types.User (UserId(UserId))
import Data.UUID hiding (null)
import Data.Types.Message
import Data.Text (Text)
import Data.Text.Encoding.Base64
import Data.Text.Encoding
import Data.Base64.Types
import Test.API ()
import Data.Time
import Data.Maybe

genChar :: MonadGen m => m Char
genChar = Gen.filterT isPrint Gen.ascii

textRange :: Range Int
textRange = Range.linear 1 10

-- Filter out ':' so that the username can be used in Basic Auth
genName :: MonadGen m => m Text
genName = Gen.text textRange $ Gen.filterT (/= ':') genChar

genText :: MonadGen m => m Text
genText = Gen.text textRange genChar

genPassword :: MonadGen m => m Text
genPassword = genText

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
mkHtmlReq method = mkReq method ("Accept", "text/html")

mkBasicAuth :: TestUser v -> Header
mkBasicAuth u =
  ( "Authorization"
  , "Basic " <> encodeUtf8
    ( extractBase64 @_ @Text $ encodeBase64 (u ^. tuName <> ":" <> u ^. tuPass)
    )
  )

mkJwtAuth :: Var String Concrete -> Header
mkJwtAuth jwt =
  ( "Authorization"
  , "Bearer " <> BS8.pack (concrete jwt)
  )

mkAuth :: Auth Concrete -> Header
mkAuth (Basic u) = mkBasicAuth u
mkAuth (Bearer j) = mkJwtAuth j

genAuth :: MonadGen gen => TestUser v -> gen (Auth v)
genAuth u = Gen.choice
  [ pure $ Basic u
  , Gen.mapMaybeT id $ pure $ Bearer <$> view tuAuth u
  ]

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
  [ Require $ \state input ->
    let uid = input ^. luId
    in maybe False (\u -> mkLoginUser uid u == input)
    $ M.lookup uid (state ^. users)
  , Update $ \state input output -> state &
    users %~ M.update (\u -> pure $ u & tuAuth .~ Just output) (input ^. luId)
  ]
  where
    gen :: TestState v -> Maybe (gen (LoginUser v))
    gen state = if M.null $ state ^. users 
      then Nothing
      else Just $ uncurry mkLoginUser <$> Gen.element (M.toList $ state ^. users)
    exe :: LoginUser Concrete -> m String
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/login"
      let req' = mkJsonReq methodPost [("Content-Type", "application/json")] req
            { H.requestBody = H.RequestBodyLBS $ encode input
            }
      res <- liftIO $ H.httpNoBody req' $ env ^. manager
      res.responseStatus === status303
      extractJwt res

getAllMessages :: forall gen m. CanStateM gen m => TestEnv -> Command gen m TestState
getAllMessages env = Command gen exe
  [ Require $ \state input -> M.member (input ^. gamId) $ state ^. users 
  , Ensure $ \_old new input output ->
    let stateMsgs = sort $ new ^. messages . ix (input ^. gamId)
        apiMsgs   = sort $ mkTestMessage <$> output
     in stateMsgs === apiMsgs
  ]
  where
    gen :: TestState v -> Maybe (gen (GetAllMessages v))
    gen state = if M.null $ state ^. users
      then Nothing
      else Just $ do
        (uid, u) <- Gen.element $ M.toList $ state ^. users
        GetAllMessages uid <$> genAuth u
    exe :: GetAllMessages Concrete -> m [Message]
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/messages/all"
      let req' = mkJsonReq methodGet [mkAuth $ input ^. gamAuth] req
      res <- liftIO $ H.httpLbs req' $ env ^. manager
      res.responseStatus === status200
      either fail pure $ eitherDecode res.responseBody

getMessages :: forall gen m. CanStateM gen m => TestEnv -> Command gen m TestState
getMessages = undefined

postMessage :: forall gen m. CanStateM gen m => TestEnv -> Command gen m TestState
postMessage env = Command gen exe
  [ Require $ \state input -> M.member (input ^. pmTo) (state ^. users)
  , Update $ \state input output -> 
    let addMessage = (
          TestMessage
            output
            (input ^. pmFrom)
            (input ^. pmTo)
            (input ^. pmBody)
          : )
    in state & messages %~ M.alter (pure . addMessage . fromMaybe []) (input ^. pmTo) 
  ]
  where
    gen :: TestState v -> Maybe (gen (PostMessage v))
    gen state = if M.null $ state ^. users
      then Nothing
      else Just $ do
        (fromId, u) <- Gen.element $ M.toList $ state ^. users
        toId <- Gen.element $ M.keys $ state ^. users
        PostMessage
          <$> genAuth u 
          <*> pure fromId
          <*> pure toId
          <*> genText
    exe :: PostMessage Concrete -> m MessageId
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/message"
      let req' = mkJsonReq methodPost
            [ mkAuth $ input ^. pmAuth
            , ("Content-Type", "application/json")
            ] $ req
              { H.requestBody = H.RequestBodyLBS $ encode input }
      res <- liftIO $ H.httpLbs req' $ env ^. manager
      res.responseStatus === status200
      either fail pure $ eitherDecode res.responseBody

getUsers :: forall gen m. CanStateM gen m => TestEnv -> Command gen m TestState
getUsers = undefined