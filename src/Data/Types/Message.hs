module Data.Types.Message where

import Data.Types.User
import Data.Text hiding (null)
import GHC.Generics (Generic)
import Data.UUID hiding (null)
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
import Web.FormUrlEncoded
import Data.Types.Auth

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

instance FromForm CreateMessage where
  fromForm f = CreateMessage
    <$> parseUnique "to" f
    <*> parseUnique "body" f

newtype AllMessages = AllMessages
  { allMessages :: [Message]
  }
instance ToJSON AllMessages where
  toJSON (AllMessages l) = toJSON l

data AuthedMessage = AuthedMessage
  { auth :: Authed
  , message :: Message
  }
instance ToJSON AuthedMessage where
  toJSON m = toJSON m.message

data DbMessage = DbMessage
  { dbMessageId   :: MessageId
  , dbMessageFrom :: UserId
  , dbMessageTo   :: UserId
  , dbMessageBody :: Text
  , dbMessageSent :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

instance FromRow DbMessage where
  fromRow = DbMessage
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field

data Message = Message
  { messageId       :: MessageId
  , messageFrom     :: UserId
  , messageFromName :: Text
  , messageTo       :: UserId
  , messageToName   :: Text
  , messageBody     :: Text
  , messageSent     :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Message where
  toJSON m = object
    [ "id"       .= m.messageId
    , "from"     .= m.messageFrom
    , "fromName" .= m.messageFromName
    , "to"       .= m.messageTo
    , "toName"   .= m.messageToName
    , "body"     .= m.messageBody
    , "sent"     .= m.messageSent
    ]

instance FromRow Message where
  fromRow = Message
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

newtype MessagePosted = MessagePosted MessageId
  deriving (Eq, Ord, Show)

instance ToJSON MessagePosted where
  toJSON (MessagePosted mid) = toJSON mid

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
  liftIO $ execute c "insert into message_sync (time, user) values (?, ?) on conflict do update set time = ?" (t, uid, t)

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
  liftIO $ query c
    "select m.id, m.from_user, u1.name, m.to_user, u2.name, m.body, m.sent \
    \from message as m \
    \join user as u1 on m.from_user = u1.id \
    \join user as u2 on m.to_user = u2.id \
    \where m.from_user = ?"
    (Only uid)

getRecentMessagesForUser :: CanAppM m c e => UserId -> m [Message]
getRecentMessagesForUser uid = do
  mSync <- getMessageSync uid
  case mSync of
    Nothing -> getAllMessagesForUser uid
    Just sync -> getMessagesForUserSince uid sync.syncTime

getMessagesForUserSince :: CanAppM m c e => UserId -> UTCTime -> m [Message]
getMessagesForUserSince uid since = do
  c <- asks conn
  liftIO $ query c
    "select m.id, m.from_user, u1.name, m.to_user, u2.name, m.body, m.sent \
    \from message as m \
    \join user as u1 on m.from_user = u1.id \
    \join user as u2 on m.to_user = u2.id \
    \where m.from_user = ? and m.sent >= ?"
    (uid, since)

writeMessage :: CanAppM m c e => UserId -> CreateMessage -> m MessageId
writeMessage from (CreateMessage to body) = do
  c <- asks conn
  mid <- MessageId <$> liftIO nextRandom
  liftIO $ execute c "insert into message (id, from_user, to_user, body, sent) values (?, ?, ?, ?, datetime())" (mid, from, to, body)
  pure mid