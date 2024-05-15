module Data.DB.Schema where

import Database.SQLite.Simple
import Data.Types.AppM
import Data.Types.Env
import Control.Monad.Reader
import Control.Monad.IO.Class

-- Overall database version
createSchemaVersion :: Query
createSchemaVersion = "create table if not exists schema_version (version integer primary key not null)"

getSchemaVersion :: Query
getSchemaVersion = "select version from schema_version"

setSchema :: Query
setSchema = "update schema_version set version = ?"

-- Get/set the JWK for signing auth tokens
getJWK :: Query
getJWK = "select value from jwk"

insertJWK :: Query
insertJWK = "insert into jwk (value) values (?)"

createSchema :: CanAppM m c e => m ()
createSchema = do
  c <- asks conn
  liftIO $ execute_ c createSchemaVersion