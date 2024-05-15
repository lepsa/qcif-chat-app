module Data.Types.AppM where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Types.Env
import Data.Types.Error (FromAppError)

type AppM m c e a = ReaderT c (ExceptT e m) a

type CanAppM m c e =
  ( MonadReader c m
  , MonadError e m
  , MonadIO m
  , HasEnv c
  , FromAppError e
  )