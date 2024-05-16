module Server where

import Data.Time (getCurrentTimeZone)
import Database.SQLite.Simple
    ( execute,
      open,
      query_,
      withTransaction,
      Only(Only)
      )
import Data.Types.Env
import Data.Types.AppM
import Data.DB.Schema
import Control.Monad.Reader
import Data.Types.Error
import Control.Monad
import Servant.Auth.Server
import Network.Wai.Handler.Warp
import Data.Proxy
import Servant.Server
import Data.Types.Auth
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Crypto.JOSE (JWK)
import Data.Kind
import Data.Types.API
import Servant hiding (BasicAuth)
import Data.Types.User qualified as U
import Data.Types.Message
import Data.Time (getCurrentTime)
import Text.Blaze ()
import Control.Monad.Trans.Except
import Data.Html.Root
import System.Directory
import System.FilePath
import qualified Data.Html.Login as H
import qualified Data.Html.Register as H
import qualified Data.Html.User ()
import Data.Types.User (UserLogin (..))
import qualified Data.Text as T
import Data.Functor
import qualified Data.Html.Message as H

-- Server runner.
-- The initial IO action is useful in tests to indicate when the server is stable.
-- `Proxy api` and the constriants helps the type inference along, otherwise it gets confused.
-- The actual server code we want to run.
runServer 
  :: (HasServer (api :: Type) '[BasicAuthCfg', CookieSettings, JWTSettings])
  => IO ()
  -> Proxy api
  -> FilePath
  -> (CookieSettings -> JWTSettings -> FilePath -> ServerT api (AppM IO Env AppError))
  -> Port
  -> IO ()
runServer onStartup api dbPath serverM port = do
  c <- open dbPath
  tz <- getCurrentTimeZone
  currentDirectory <- getCurrentDirectory
  let conf = Env c tz
  either dbErr pure <=< runAppM @_ @_ @AppError conf $ do
    initDb
    createSchema
    migrate
  jwtKey <- getJwtKey conf
  let jwtSettings = defaultJWTSettings jwtKey
      cookieSettings = defaultCookieSettings
        { cookieXsrfSetting = pure $ defaultXsrfCookieSettings
          { xsrfExcludeGet = True
          }
        , cookieMaxAge = pure $ 7 * 24 * 60 * 60 -- 7 days to seconds
        }
      -- Basic auth checks the user/password each time, so it already
      -- handles a user being deleted between user requests.
      cfg = BasicAuthCfg' (conn conf) :. cookieSettings :. jwtSettings :. EmptyContext
      warpSettings = setBeforeMainLoop onStartup
        $ setHost "*6"
        $ setPort port defaultSettings
  runSettings warpSettings $
    serveWithContext api cfg $
      hoistServerWithContext api
        (Proxy @'[BasicAuthCfg', CookieSettings, JWTSettings])
        (runToHandler conf) $
        serverM cookieSettings jwtSettings currentDirectory
  where
    dbErr e = error $ "An error occurred while setting up the database: " <> show e
    runToHandler :: Env -> AppM IO Env AppError a -> Handler a
    runToHandler conf m = do
      e <- liftIO $ runAppM conf m
      either throwError_ pure e

getJwtKey :: HasEnv c => c -> IO JWK
getJwtKey conf = do
  withTransaction c $ do
    jsons <- query_ c getJWK
    case jsons of
      [] -> do
        putStrLn "No JWK found, making a new one."
        jwk <- generateKey
        execute c insertJWK (Only $ BSL8.unpack $ encode jwk)
        pure jwk
      [Only json] -> do
        putStrLn "Found a JWK entry, decoding"
        case eitherDecode $ BSL8.pack json of
          Right jwk -> pure jwk
          Left s    -> error s
      _ -> error "Too many JWKs in the database"
  where
    c = conn conf

server :: CookieSettings -> JWTSettings -> FilePath -> ServerT TopAPI (AppM IO Env AppError)
server cookieSettings jwtSettings currentDirectory =
    mainServer cookieSettings jwtSettings
      :<|> serveDirectoryWebApp (currentDirectory </> "static")
    
mainServer :: CookieSettings -> JWTSettings -> Authed -> ServerT MainAPI (AppM IO Env AppError)
mainServer cookieSettings jwtSettings auth = htmlServer auth :<|> coreServer cookieSettings jwtSettings auth

htmlServer :: Authed -> ServerT HtmlAPI (AppM IO Env AppError)
htmlServer auth =
       pure (root auth)
  :<|> pure (H.login auth)
  :<|> pure (H.register auth)
  :<|> protected auth (\userLogin -> H.newMessage auth userLogin <$> U.getUsers)

coreServer :: CookieSettings -> JWTSettings -> Authed -> ServerT CoreAPI (AppM IO Env AppError)
coreServer cookieSettings jwtSettings a =
       login
  :<|> register
  :<|> protected a getMessages
  :<|> protected a getAllMessages
  :<|> protected a . flip postMessage
  :<|> protected a getUsers
  where
    getMessages user = do
      t <- liftIO getCurrentTime
      AuthedValue a <$> getSyncedMessages t user.userLoginId
    getAllMessages user = AuthedValue a . AllMessages <$> getAllMessagesForUser user.userLoginId
    postMessage user msg = MessagePosted <$> writeMessage user.userLoginId msg
    getUsers _user = AuthedValue a <$> U.getUsers
    login l = do
      c <- asks conn
      user <- either throwError_ pure <=< runExceptT $ checkUserPassword c l.loginUser l.loginPass
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing -> throwError_ $ Other "Could not apply login cookies"
        Just cookies -> pure
          $ cookies
          $ addHeader (linkText $ Proxy @(AuthLogin :> GetRoot))
          $ addHeader (T.pack $ show user.userLoginId) ()
    register createUser = do
      user <- U.addUser createUser <&> \u -> UserLogin u.userId u.userName
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing -> throwError_ $ Other "Could not apply login cookies"
        Just cookies -> pure
          $ cookies
          $ addHeader (linkText $ Proxy @(AuthLogin :> GetRoot))
          $ addHeader (T.pack $ show user.userLoginId.unUserId) ()

protected :: Authed -> (UserLogin -> AppM IO Env AppError a) -> AppM IO Env AppError a
protected (Authenticated user) f = f user
protected _ _ = throwError_ BadAuth

serverNat :: AsError e' e => AppM IO Env e a -> AppM IO Env e' a
serverNat n = do
  c <- ask
  e <- liftIO $ runAppM c n
  either throwError_ pure e