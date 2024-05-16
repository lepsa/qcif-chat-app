{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Test.Types where

import Control.Lens hiding ((.=))
import Network.HTTP.Client
import Hedgehog
import Data.Map (Map)
import Data.Types.User (UserId)
import GHC.Generics
import Data.Types.Message
    ( Message(messageBody, messageId, messageFrom, messageTo),
      MessageId )
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Time
import Data.Text (Text)

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

data TestEnv = TestEnv
  { _manager :: Manager
  , _baseUrl :: String
  }
makeLenses ''TestEnv

data TestUser v = TestUser
  { _tuName :: Text
  , _tuPass :: Text
  , _tuAuth :: Maybe (Var String v)
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB TestUser
instance TraversableB TestUser
makeLenses ''TestUser

data TestMessage v = TestMessage
  { _tmId   :: Var MessageId v
  , _tmFrom :: Var UserId v
  , _tmTo   :: Var UserId v
  , _tmBody :: Text
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB TestMessage
instance TraversableB TestMessage
makeLenses ''TestMessage

mkTestMessage :: Message -> TestMessage Concrete
mkTestMessage msg = TestMessage
  (Var $ Concrete $ msg.messageId)
  (Var $ Concrete $ msg.messageFrom)
  (Var $ Concrete $ msg.messageTo)
  msg.messageBody

data TestState v = TestState
  { _users :: Map (Var UserId v) (TestUser v)
  , _messages :: Map (Var UserId v) [TestMessage v]
  , _messaegeSync :: Map (Var UserId v) (Var UTCTime v)
  } deriving (Eq, Ord, Show, Generic)
makeLenses ''TestState

data RegisterUser v = RegisterUser
  { _ruName :: Text
  , _ruPass :: Text
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB RegisterUser
instance TraversableB RegisterUser
makeLenses ''RegisterUser

instance ToJSON (RegisterUser v) where
  toJSON r = object
    [ "user"     .= view ruName r
    , "password" .= view ruPass r
    ]


class HasUserId a v | a -> v where
  userId :: Lens' a (Var UserId v)

data LoginUser v = LoginUser
  { _luId   :: Var UserId v
  , _luName :: Text
  , _luPass :: Text
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB LoginUser
instance TraversableB LoginUser
makeLenses ''LoginUser

instance HasUserId (LoginUser v) v where
  userId = luId

instance ToJSON (LoginUser v) where
  toJSON l = object
    [ "user"     .= view luName l
    , "password" .= view luPass l
    ]

mkLoginUser :: Var UserId v -> TestUser v -> LoginUser v
mkLoginUser uid u = LoginUser uid (u ^. tuName) (u ^. tuPass)

data Auth v
  = Basic (Var UserId v) (TestUser v)
  | Bearer (Var UserId v) (Var String v)
  deriving (Eq, Ord, Show, Generic)
instance FunctorB Auth
instance TraversableB Auth

class HasAuth a v | a -> v where
  auth :: Lens' a (Auth v)
instance HasUserId (Auth v) v where
  userId = lens (\case
      Basic uid _ -> uid
      Bearer uid _ -> uid
    ) (\a uid -> case a of
      Basic _ x -> Basic uid x
      Bearer _ x -> Bearer uid x
    )
instance HasAuth (Auth v) v where
  auth = id

newtype GetAllMessages v = GetAllMessages
  { _gamAuth :: Auth v
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB GetAllMessages
instance TraversableB GetAllMessages
makeLenses ''GetAllMessages

instance HasAuth (GetAllMessages v) v where
  auth = gamAuth
instance HasUserId (GetAllMessages v) v where
  userId = auth . userId

data PostMessage v = PostMessage
  { _pmAuth :: Auth v
  , _pmTo   :: Var UserId v
  , _pmBody :: Text
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB PostMessage
instance TraversableB PostMessage
makeLenses ''PostMessage
instance ToJSON (PostMessage Concrete) where
  toJSON pm = object
    [ "to"   .= concrete (view pmTo pm)
    , "body" .= view pmBody pm
    ]
instance HasAuth (PostMessage v) v where
  auth = pmAuth
instance HasUserId (PostMessage v) v where
  userId = auth . userId
