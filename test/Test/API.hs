module Test.API where

import Data.Types.API
import Data.Data
import Server

type TestTopAPI = TopAPI

testTopAPI :: Proxy TestTopAPI
testTopAPI = Proxy

testServer = server