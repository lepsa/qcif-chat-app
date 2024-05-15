module Data.Types.Error where

data AppError = AppError
  deriving (Eq, Ord, Show)
instance FromAppError AppError where
  fromAppError = id
instance ToAppError AppError where
  toAppError = id

class FromAppError e where
  fromAppError :: AppError -> e
class ToAppError e where
  toAppError :: e -> AppError