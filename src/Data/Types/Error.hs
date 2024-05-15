module Data.Types.Error where
import Control.Monad.Except

data AppError
  = AppError
  | DB DBError
  deriving (Eq, Ord, Show)
instance FromAppError AppError where
  fromAppError = id
instance ToAppError AppError where
  toAppError = id

class FromAppError e where
  fromAppError :: AppError -> e
class ToAppError e where
  toAppError :: e -> AppError

data DBError
  = NotFound
  | TooManyResults
  | InsertionFailure
  | Other String
  deriving (Eq, Ord, Show)

throwError_ :: (FromAppError e, MonadError e m) => AppError -> m a
throwError_ = throwError . fromAppError