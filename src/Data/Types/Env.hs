module Data.Types.Env where

import Database.SQLite.Simple
import Data.Time

data Env = Env
  { envConn :: Connection
  , envTimeZone :: TimeZone
  }
instance HasEnv Env where
  env = id

class HasEnv c where
  {-# MINIMAL (env | conn, timeZone) #-}
  env :: c -> Env
  conn :: c -> Connection
  conn = envConn . env
  timeZone :: c -> TimeZone
  timeZone = envTimeZone . env