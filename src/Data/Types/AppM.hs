module Data.Types.AppM where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Types.Env
import Data.Types.Error
import Servant

type AppM m c e = ReaderT c (ExceptT e m)

type CanAppM m c e =
  ( MonadReader c m
  , MonadError e m
  , MonadIO m
  , HasEnv c
  , AsError e AppError
  , AsError e DBError
  , AsError e ServerError
  )

runAppM :: forall m c e a. c -> AppM m c e a -> m (Either e a)
runAppM c m = runExceptT $ runReaderT m c