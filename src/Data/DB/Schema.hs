{-# LANGUAGE ScopedTypeVariables #-}

module Data.DB.Schema where

import Database.SQLite.Simple
import Data.Types.AppM
import Data.Types.Env
import Control.Monad.Reader
import Database.SQLite.Simple.ToField
import Data.Types.Error
import Data.Foldable
import Data.List

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

newtype SchemaVersion = SchemaVersion Int
  deriving (Eq, Ord, Show, Read, Num)

instance FromRow SchemaVersion where
  fromRow = SchemaVersion <$> field

instance ToRow SchemaVersion where
  toRow (SchemaVersion v) = pure $ toField v

initDb :: CanAppM m c e => m ()
initDb = do
  _c <- asks conn
  -- Any tasks needed for DB setup that aren't related to the schema.
  -- For sqlite this would include PRAGMA checks and VACUUM commands.
  pure ()

migrate :: CanAppM m c e => m ()
migrate = do
  c <- asks conn
  versions <- liftIO $ withTransaction c $ query_ c getSchemaVersion
  version <- case versions of
    []  -> pure 0
    [v] -> pure v
    _   -> throwError_ $ DB $ Other "Could not get a unique schema version"
  let migrationsToRun = filter (\(v, _) -> v >= version) $ sortBy comp migrations
  liftIO $ traverse_
    (withExclusiveTransaction c . runMigration c)
    migrationsToRun
  pure ()
  where
    comp (v1, _) (v2, _) = compare v1 v2

runMigration :: Connection -> (SchemaVersion, [Query]) -> IO ()
runMigration c (version, queries) = do
  -- Run the queries
  traverse_ (execute_ c) queries
  -- Bump the version for the migration
  execute c setSchema $ version + 1

migrations :: [(SchemaVersion, [Query])]
migrations = []