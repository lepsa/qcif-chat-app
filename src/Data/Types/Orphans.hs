{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Types.Orphans where

import Data.UUID
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Control.Monad
import Database.SQLite.Simple.FromRow

instance FromField UUID where
  fromField = fromField @String >=> maybe (fail "Could not parse UUID") pure . fromString
instance FromRow UUID where
  fromRow = fieldWith fromField
instance ToField UUID where
  toField = toField @String . toString
