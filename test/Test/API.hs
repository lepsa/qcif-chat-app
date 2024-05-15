{-# OPTIONS_GHC -Wno-orphans #-}
module Test.API where

import Data.Types.API
import Data.Data
import Server
import Servant
import Data.Types.AppM
import Data.Types.Env
import Servant.Auth.Server
import Control.Monad.Reader
import Data.Foldable
import Data.Types.Error
import Database.SQLite.Simple
import Data.Aeson
import Data.Types.Message

type TestTopAPI = TopAPI :<|> TestAPI

type TestAPI =
  "reset" :> PostNoContent

testTopAPI :: Proxy TestTopAPI
testTopAPI = Proxy

testTopServer
  :: CookieSettings
  -> JWTSettings
  -> FilePath
  -> ServerT TestTopAPI (AppM IO Env AppError)
testTopServer cs js cd = server cs js cd :<|> testServer

testServer :: ServerT TestAPI (AppM IO Env AppError)
testServer = NoContent <$ resetDb

resetDb :: AppM IO Env AppError ()
resetDb = do
  c <- asks envConn
  liftIO $ traverse_ (execute_ c)
    [ "delete from user"
    , "delete from user_pass"
    , "delete from message"
    , "delete from message_sync"
    ]

instance FromJSON MessageId where
  parseJSON v = MessageId <$> parseJSON v

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> Message
    <$> o .: "id"
    <*> o .: "from"
    <*> o .: "fromName"
    <*> o .: "to"
    <*> o .: "toName"
    <*> o .: "body"
    <*> o .: "sent"