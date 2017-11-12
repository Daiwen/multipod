module Import.Network
    ( getBody
    , NetworkError
    ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Network.HTTP
import Network.Stream

newtype NetworkError =
    NetworkError ConnError
    deriving (Eq, Show)

instance Exception NetworkError

getBody :: (MonadIO m, MonadThrow m) => String -> m String
getBody address = do
    result <- liftIO $ simpleHTTP (getRequest address)
    case result of
        Left e -> throwM $ NetworkError e
        Right s -> return $ rspBody s
