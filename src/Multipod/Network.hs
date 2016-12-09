module Multipod.Network
  ( requestBody
  , NetworkError
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Network.Stream
import Network.HTTP

data NetworkError =
  NetworkError ConnError
  deriving (Eq, Show)

instance Exception NetworkError

requestBody
  :: (MonadIO m, MonadThrow m)
  => String -> m String
requestBody address = do
  result <- liftIO $ simpleHTTP (getRequest address)
  case result of
    Left e -> throwM $ NetworkError e
    Right s -> return $ rspBody s
