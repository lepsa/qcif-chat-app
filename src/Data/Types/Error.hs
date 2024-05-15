module Data.Types.Error where
import Control.Monad.Except

data AppError
  = AppError
  | DB DBError
  deriving (Eq, Ord, Show)

class AsAppError e a where
  fromAppError :: a -> e
  toAppError :: e -> Maybe a

instance AsAppError e e where
  fromAppError = id
  toAppError = pure

data DBError
  = NotFound
  | TooManyResults
  | InsertionFailure
  | Other String
  deriving (Eq, Ord, Show)

instance AsAppError AppError DBError where
  fromAppError = fromAppError . DB
  toAppError (DB e) = pure e
  toAppError _ = Nothing

throwError_ :: (MonadError e m, AsAppError e e') => e' -> m a
throwError_ = throwError . fromAppError

singleResult :: (AsAppError e DBError, MonadError e m) => [a] -> m a
singleResult [] = throwError_ NotFound
singleResult [a] = pure a
singleResult _ = throwError_ TooManyResults