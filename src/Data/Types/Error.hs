module Data.Types.Error where

import Control.Monad.Except
import Servant.Auth.Server
import Servant.Server hiding (BadPassword)
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad

data AppError
  = BadAuth
  | DB DBError
  | Servant ServerError
  deriving Show

class AsError e a where
  fromError :: a -> e
  toError :: e -> Maybe a

instance AsError e e where
  fromError = id
  toError = pure

data DBError
  = NotFound
  | TooManyResults
  | InsertionFailure
  | Other String
  | DBException SomeException
  deriving Show

instance AsError AppError DBError where
  fromError = fromError . DB
  toError (DB e) = pure e
  toError _ = Nothing

instance AsError AppError ServerError where
  fromError = fromError . Servant
  toError (Servant e) = pure e
  toError _ = Nothing

instance AsError (AuthResult a) DBError where
  fromError = const BadPassword
  toError _ = Nothing

instance AsError AppError (AuthResult a) where
  fromError _ = BadAuth
  toError _ = Nothing

throwError_ :: (MonadError e m, AsError e e') => e' -> m a
throwError_ = throwError . fromError

singleResult :: (AsError e DBError, MonadError e m) => [a] -> m a
singleResult [] = throwError_ NotFound
singleResult [a] = pure a
singleResult _ = throwError_ TooManyResults

catchDbException :: (MonadError e m, AsError e DBError, MonadIO m) => IO a -> m a
catchDbException = either (throwError_ . DBException) pure <=< liftIO . try