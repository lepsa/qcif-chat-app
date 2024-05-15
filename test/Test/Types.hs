module Test.Types where

import Network.HTTP.Client
import Hedgehog
import Data.Map (Map)
import Data.Map qualified as M
import Data.Types.User
import GHC.Generics
import Data.Types.Message

data TestEnv = TestEnv
  { manager :: Manager
  , baseUrl :: String
  }

data TestState v = TestState
  { users :: Map (Var UserId v) (TestUser v)
  , messages :: Map (Var MessageId v) (TestMessage v)
  }

initialState :: TestState v
initialState = TestState M.empty M.empty

data TestUser v = TestUser
  { testUserName :: String
  , testUserPass :: String
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB TestUser
instance TraversableB TestUser

data TestMessage v = TestMessage
  { dbMessageFrom :: Var UserId v
  , dbMessageTo   :: Var UserId v
  , dbMessageBody :: String
  } deriving (Eq, Ord, Show, Generic)
instance FunctorB TestMessage
instance TraversableB TestMessage