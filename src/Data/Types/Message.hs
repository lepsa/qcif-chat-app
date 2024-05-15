module Data.Types.Message where

import Data.Types.User
import Data.Text
import GHC.Generics

data Message = Message
  { messageFrom :: UserId
  , messageTo   :: [UserId]
  , messageBody :: Text
  } deriving (Eq, Ord, Show, Generic)