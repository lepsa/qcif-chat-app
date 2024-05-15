module Main where

import Data.Types.API
import Server

main :: IO ()
main = runServer (pure ()) topAPI "chat-server.db" server 8080
