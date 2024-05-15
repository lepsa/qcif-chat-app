module Data.Types.Error where

import Control.Monad.Except
import Servant.Auth.Server
import Servant.Server hiding (BadPassword)

data AppError
  = BadAuth
  | DB DBError
  | Servant ServerError
  deriving (Eq, Show)

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
  deriving (Eq, Ord, Show)

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

instance AsError ServerError AppError where
  fromError (Servant e) = e
  fromError BadAuth = err401
  fromError (DB _) = err500
  toError = pure . Servant

throwError_ :: (MonadError e m, AsError e e') => e' -> m a
throwError_ = throwError . fromError

singleResult :: (AsError e DBError, MonadError e m) => [a] -> m a
singleResult [] = throwError_ NotFound
singleResult [a] = pure a
singleResult _ = throwError_ TooManyResults