module Data.Types.User where

import Data.UUID
import Data.Text
import Data.Aeson
import GHC.Generics

data User = User
  { userId :: UUID
  , userName :: Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON User where
  toJSON u = object
    [ "id" .= u.userId
    , "name" .= u.userName
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "id"
    <*> o .: "id"