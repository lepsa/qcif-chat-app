module Main where

import Data.Types.API
import Server

main :: IO ()
main = runServer (pure ()) topAPI "./db/chat-server.db" server 8080
