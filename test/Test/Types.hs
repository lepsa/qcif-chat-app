{-# LANGUAGE TemplateHaskell #-}

module Test.Types where

import Control.Lens hiding ((.=))
import Network.HTTP.Client
import Hedgehog
import Data.Map (Map)
import Data.Types.User
import GHC.Generics
import Data.Types.Message
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Time
import qualified Data.Text as T
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
  { _tmFrom :: Var UserId v
  , _tmTo   :: Var UserId v
  , _tmBody :: String
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB TestMessage
instance TraversableB TestMessage
makeLenses ''TestMessage

mkTestMessage :: Message -> TestMessage Concrete
mkTestMessage msg = TestMessage
  (Var $ Concrete $ msg.messageFrom)
  (Var $ Concrete $ msg.messageTo)
  (T.unpack msg.messageBody)

data TestState v = TestState
  { _users :: Map (Var UserId v) (TestUser v)
  , _messages :: Map (Var UserId v) [TestMessage v]
  , _messaegeSync :: Map (Var UserId v) (Var UTCTime v)
  }
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

data LoginUser v = LoginUser
  { _luId   :: Var UserId v
  , _luName :: Text
  , _luPass :: Text
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB LoginUser
instance TraversableB LoginUser
makeLenses ''LoginUser

instance ToJSON (LoginUser v) where
  toJSON l = object
    [ "user"     .= view luName l
    , "password" .= view luPass l
    ]

data Auth v
  = Basic (TestUser v)
  | Bearer (Var String v)
  deriving (Eq, Ord, Show, Generic)
instance FunctorB Auth
instance TraversableB Auth

data GetAllMessages v = GetAllMessages
  { _gamId   :: Var UserId v
  , _gamAuth :: Auth v
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB GetAllMessages
instance TraversableB GetAllMessages
makeLenses ''GetAllMessages
