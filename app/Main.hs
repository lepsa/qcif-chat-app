module Main where
import Data.Types.API
import Server

main :: IO ()
main = runServer (pure ()) topAPI server 8080
