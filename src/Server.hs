module Server where

import Data.Time (getCurrentTimeZone)
import Database.SQLite.Simple
import Data.Types.Env
import Data.Types.AppM
import Data.DB.Schema
import Data.Types.Error
import Control.Monad

server :: IO ()
server = do
  c <- open dbPath
  tz <- getCurrentTimeZone
  let conf = Env c tz
  either dbErr pure <=< runAppM @_ @_ @AppError conf $ do
    initDb
    createSchema
    migrate
  pure ()
  where
    dbPath = "chat-server.db"
    dbErr e = error $ "An error occurred while setting up the database: " <> show e