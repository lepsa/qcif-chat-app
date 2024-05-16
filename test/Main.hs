module Main where

import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Types      as H
import Control.Concurrent
import Server
import Test.API
import Test.Types
import Test.StateMachine
import Hedgehog
import System.Directory

main :: IO Bool
main = do
  ready <- newEmptyMVar
  let onStart = putMVar ready ()
      port = 8081
  createDirectoryIfMissing False "./db"
  serverThread <- forkIO $ runServer onStart testTopAPI "./db/test-server.db" testTopServer port
  takeMVar ready

  mgr <- H.newManager H.defaultManagerSettings
  let url = "http://localhost:" <> show port
      env = TestEnv mgr url
      reset = do
        req <- H.parseRequest $ url <> "/reset"
        let req' = req { H.method = H.methodPost }
        res <- H.httpNoBody req' mgr
        pure $ res.responseStatus == H.status204
  results <- checkParallel $ Group "API Tests"
    [ ("API State Machine", propApiTests env reset)
    ]
  killThread serverThread
  pure results