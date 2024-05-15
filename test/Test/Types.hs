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

type CanStateM gen m = (MonadGen gen, MonadFail m, MonadThrow m, MonadIO m, MonadTest m)

data TestEnv = TestEnv
  { _manager :: Manager
  , _baseUrl :: String
  }
makeLenses ''TestEnv

data TestUser v = TestUser
  { _tuName :: String
  , _tuPass :: String
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

data TestState v = TestState
  { _users :: Map (Var UserId v) (TestUser v)
  , _messages :: Map (Var UserId v) (TestMessage v)
  }
makeLenses ''TestState

data RegisterUser v = RegisterUser
  { _ruName :: String
  , _ruPass :: String
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
  , _luName :: String
  , _luPass :: String
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB LoginUser
instance TraversableB LoginUser
makeLenses ''LoginUser

instance ToJSON (LoginUser v) where
  toJSON l = object
    [ "user"     .= view luName l
    , "password" .= view luPass l
    ]