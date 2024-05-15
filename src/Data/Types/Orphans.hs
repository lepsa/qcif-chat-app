{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Types.Orphans where

import Data.UUID
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Control.Monad
import Database.SQLite.Simple.FromRow
import Data.Password.Argon2
import Data.Text

instance FromField UUID where
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString
instance FromRow UUID where
  fromRow = fieldWith fromField
instance ToField UUID where
  toField = toField @String . toString

instance FromField (PasswordHash Argon2) where
  fromField f = PasswordHash <$> fromField @Text f

instance FromRow (PasswordHash Argon2) where
  fromRow = PasswordHash <$> field @Text

instance ToField (PasswordHash Argon2) where
  toField = toField . unPasswordHash