module Data.Types.Message where

import Data.Types.User
import Data.Text
import GHC.Generics (Generic)
import Data.UUID
import Data.Types.AppM
import Database.SQLite.Simple
import Data.Types.Env
import Control.Monad.Reader
import Database.SQLite.Simple.FromField
import Data.Types.Orphans ()
import Data.Time
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple.ToField
import Data.Types.Error
import Data.Aeson

newtype MessageId = MessageId { unMessageId :: UUID }
  deriving (Eq, Ord, Show, Generic)
instance FromField MessageId where
  fromField f = MessageId <$> fromField f
instance ToField MessageId where
  toField = toField . unMessageId
instance ToJSON MessageId where
  toJSON = toJSON . unMessageId

data CreateMessage = CreateMessage
  { createMessageTo   :: UserId
  , createMessageBody :: Text
  } deriving (Eq, Ord, Show, Generic)
instance FromJSON CreateMessage where
  parseJSON = withObject "CreateMessage" $ \o -> CreateMessage
    <$> o .: "to"
    <*> o .: "body"

data Message = Message
  { messageId   :: MessageId
  , messageFrom :: UserId
  -- TODO: Can this be made into a NonEmpty so we can do group messaging?
  , messageTo   :: UserId
  , messageBody :: Text
  , messageSent :: UTCTime
  } deriving (Eq, Ord, Show, Generic)
instance FromRow Message where
  fromRow = Message
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
instance ToJSON Message where
  toJSON m = object
    [ "id" .= m.messageId
    , "from" .= m.messageFrom
    , "to" .= m.messageTo
    , "body" .= m.messageBody
    , "sent" .= m.messageSent
    ]

-- Store the last time a user requested their messages.
data MessageSync = MessageSync
  { syncUser :: UserId
  , syncTime :: UTCTime
  } deriving (Eq, Ord, Show, Generic)
instance FromRow MessageSync where
  fromRow = MessageSync <$> field <*> field

getSyncedMessages :: forall m c e. CanAppM m c e => UTCTime -> UserId -> m [Message]
getSyncedMessages t uid = do
  conf <- ask
  let c = conn conf
  e <- liftIO $ withTransaction c $ runAppM @IO @c @e conf $ do
    l <- getRecentMessagesForUser uid
    setMessageSync t uid
    pure l
  either throwError_ pure e

setMessageSync :: CanAppM m c e => UTCTime -> UserId -> m ()
setMessageSync t uid = do
  c <- asks conn
  liftIO $ execute c "update message_sync set time = ? where user = ?" (t, uid)

getMessageSync :: CanAppM m c e => UserId -> m (Maybe MessageSync)
getMessageSync uid = do
  c <- asks conn
  l <- liftIO $ query c "select user, time from message_sync where user = ?" (Only uid)
  case l of
    [] -> pure Nothing
    [sync] -> pure $ Just sync
    _ -> throwError_ $ DB $ TooManyResults

getAllMessagesForUser :: CanAppM m c e => UserId -> m [Message]
getAllMessagesForUser uid = do
  c <- asks conn
  liftIO $ query c "select id, from, to, body, sent from message where from = ?" (Only uid)

getRecentMessagesForUser :: CanAppM m c e => UserId -> m [Message]
getRecentMessagesForUser uid = do
  mSync <- getMessageSync uid
  case mSync of
    Nothing -> getAllMessagesForUser uid
    Just sync -> getMessagesForUserSince uid sync.syncTime

getMessagesForUserSince :: CanAppM m c e => UserId -> UTCTime -> m [Message]
getMessagesForUserSince uid since = do
  c <- asks conn
  liftIO $ query c "select id, from, to, body, sent from message where from = ? and sent >= ?" (uid, since)

writeMessage :: CanAppM m c e => UserId -> CreateMessage -> m MessageId
writeMessage from (CreateMessage to body) = do
  c <- asks conn
  mid <- MessageId <$> liftIO nextRandom
  liftIO $ execute c "insert into message (id, from, to, body, sent) values (?, ?, ?, ?, datetime())" (mid, from, to, body)
  pure mid