module Data.Types.User where

import Data.UUID
import Data.Text
import Data.Aeson
import GHC.Generics

-- What we include in JWTs. Make it as small as possible,
-- and don't store anything that can change between requests.
newtype UserId = UserId { unUserId :: UUID }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserId where
  toJSON = toJSON . unUserId
instance FromJSON UserId where
  parseJSON v = UserId <$> parseJSON v

data User = User
  { userId :: UserId
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