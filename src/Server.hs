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
import Data.Html.Message

-- Server runner.
-- The initial IO action is useful in tests to indicate when the server is stable.
-- `Proxy api` and the constriants helps the type inference along, otherwise it gets confused.
-- The actual server code we want to run.
runServer 
  :: (HasServer (api :: Type) '[BasicAuthCfg', CookieSettings, JWTSettings])
  => IO ()
  -> Proxy api
  -> (CookieSettings -> JWTSettings -> ServerT api (AppM IO Env AppError))
  -> Port
  -> IO ()
runServer onStartup api serverM port = do
  c <- open dbPath
  tz <- getCurrentTimeZone
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
        serverM cookieSettings jwtSettings
  where
    dbPath = "chat-server.db"
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

server :: CookieSettings -> JWTSettings -> ServerT TopAPI (AppM IO Env AppError)
server cookieSettings jwtSettings =
    mainServer
  :<|> loginPage
  :<|> login
  :<|> register
  where
    loginPage = pure mempty
    login l = do
      c <- asks conn
      uid <- either throwError_ pure <=< runExceptT $ checkUserPassword c l.loginUser l.loginPass
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings uid
      case mApplyCookies of
        Nothing -> throwError_ $ Other "Could not apply login cookies"
        Just cookies -> pure $ cookies ()
    register createUser = do
      uid <- U.addUser createUser
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings uid
      case mApplyCookies of
        Nothing -> throwError_ $ Other "Could not apply login cookies"
        Just cookies -> pure $ cookies ()

mainServer :: Authed -> ServerT MainAPI (AppM IO Env AppError)
mainServer auth = htmlServer auth :<|> coreServer auth

htmlServer :: Authed -> ServerT HtmlAPI (AppM IO Env AppError)
htmlServer auth = root auth :<|> newMessage auth

coreServer :: Authed -> ServerT CoreAPI (AppM IO Env AppError)
coreServer (Authenticated uid) =
       getMessages
  :<|> getAllMessages
  :<|> postMessage
  :<|> getUsers
  where
    getMessages = do
      t <- liftIO getCurrentTime
      getSyncedMessages t uid
    getAllMessages = getAllMessagesForUser uid
    postMessage msg = void $ writeMessage uid msg
    getUsers = U.getUsers
coreServer _ = hoistServer (Proxy @CoreAPI) serverNat $ throwAll err401

serverNat :: AsError e' e => AppM IO Env e a -> AppM IO Env e' a
serverNat n = do
  c <- ask
  e <- liftIO $ runAppM c n
  either throwError_ pure e