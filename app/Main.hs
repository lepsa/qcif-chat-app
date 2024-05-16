module Main where

import Data.Types.API
import Server
import System.Directory

main :: IO ()
main = do
  createDirectoryIfMissing False "./db"
  runServer (pure ()) topAPI "./db/chat-server.db" server 8080
