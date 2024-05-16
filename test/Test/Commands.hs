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
import Data.Types.Message (Message, MessageId)
import Data.Text (Text)
import Data.Text.Encoding.Base64
import Data.Text.Encoding
import Data.Base64.Types
import Test.API ()
import Data.Time
import Data.Maybe
import Control.Monad
import Data.Types.User (User)
import Data.Types.User (User(User))

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
mkAuth (Basic _ u) = mkBasicAuth u
mkAuth (Bearer _ j) = mkJwtAuth j

genAuth :: MonadGen gen => Var UserId v -> TestUser v -> gen (Auth v)
genAuth uid u = 
  case u ^. tuAuth of
    Nothing -> pure $ Basic uid u
    Just jwt -> Gen.element
      [ Basic uid u
      , Bearer uid jwt
      ]

requireUserAuth :: (HasUserId (input v) v, HasAuth (input v) v, Ord1 v, Eq1 v) => TestState v -> input v -> Bool
requireUserAuth state input =
  case M.lookup (input ^. userId) $ state ^. users of
    Nothing -> False
    Just u -> case input ^. auth of
      Basic _ u' -> u == u'
      Bearer _ jwt -> pure jwt == u ^. tuAuth

registerUser :: forall gen m. (CanStateM gen m) => TestEnv -> Command gen m TestState
registerUser env = Command gen exe
  [ Require $ \state input ->
    let inName = input ^. ruName
        stateNames = view tuName <$> view users state
    in inName `notElem` stateNames,
    Update $ \state input output -> state &
    users %~ M.insert output (TestUser (input ^. ruName) (input ^. ruPass) Nothing)
  ]
  where
    gen :: TestState v -> Maybe (gen (RegisterUser v))
    gen state = do
      let stateNames = view tuName <$> view users state
      Just $ RegisterUser
        <$> Gen.filterT (`notElem` stateNames) genName
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
    let uid = input ^. userId
    in maybe False (\u -> mkLoginUser uid u == input)
    $ M.lookup uid (state ^. users)
  , Update $ \state input output -> state &
    users %~ M.update (\u -> pure $ u & tuAuth .~ Just output) (input ^. userId)
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
  [ Require requireUserAuth
  , Ensure $ \_old new input output -> do
    let stateMsgs = sort $ new ^. messages . ix (input ^. userId)
        apiMsgs   = sort $ mkTestMessage <$> output
    annotate $ show new
    stateMsgs === apiMsgs
  ]
  where
    gen :: TestState v -> Maybe (gen (GetAllMessages v))
    gen state = if M.null $ state ^. users
      then Nothing
      else Just $ do
        a <- uncurry genAuth <=< Gen.element $ M.toList $ state ^. users
        pure $ GetAllMessages a
    exe :: GetAllMessages Concrete -> m [Message]
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/messages/all"
      let req' = mkJsonReq methodGet [mkAuth $ input ^. gamAuth] req
      res <- liftIO $ H.httpLbs req' $ env ^. manager
      annotate $ show res
      res.responseStatus === status200
      either fail pure $ eitherDecode res.responseBody

getMessages :: forall gen m. CanStateM gen m => TestEnv -> Command gen m TestState
getMessages = undefined

postMessage :: forall gen m. CanStateM gen m => TestEnv -> Command gen m TestState
postMessage env = Command gen exe
  [ Require requireUserAuth
  , Update $ \state input output -> 
    let addMessage = (
          TestMessage
            output
            (input ^. userId)
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
        a <- uncurry genAuth <=< Gen.element $ M.toList $ state ^. users
        toId <- Gen.element $ M.keys $ state ^. users
        PostMessage
          <$> pure a
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
getUsers env = Command gen exe
  [ Require requireUserAuth
  , Ensure $ \_old new _input output -> do
    let modelUsers :: [(Var UserId Concrete, TestUser Concrete)]
        modelUsers = cleanModelUser <$> M.toList (new ^. users)
        cleanModelUser (uid, u) = (uid, u & tuPass .~ mempty & tuAuth .~ Nothing)
        mkTestUser (User uid n) = (Var $ Concrete uid, TestUser n mempty Nothing)
    sort modelUsers === sort (mkTestUser <$> output)
  ]
  where
    gen :: TestState v -> Maybe (gen (GetUsers v))
    gen state = if M.null $ state ^. users
      then Nothing
      else Just $ do
        a <- uncurry genAuth <=< Gen.element $ M.toList $ state ^. users
        pure $ GetUsers a
    exe :: GetUsers Concrete -> m [User]
    exe input = do
      req <- H.parseRequest $ view baseUrl env <> "/users"
      let req' = mkJsonReq methodGet [mkAuth $ input ^. auth] req
      res <- liftIO $ H.httpLbs req' $ env ^. manager
      res.responseStatus === status200
      either fail pure $ eitherDecode res.responseBody