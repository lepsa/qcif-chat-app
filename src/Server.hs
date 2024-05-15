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
runServer onStartup api server port = do
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
        server cookieSettings jwtSettings
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
