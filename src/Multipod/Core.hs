{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Multipod.Core where

import Network.Stream
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Text

import Multipod.PodcastData
import Multipod.PodcastReader

newtype CoreMonad a = CMo
  { runCore :: (LoggingT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch,
              MonadBase IO, MonadLogger)

runCoreMonad :: CoreMonad a -> IO a
runCoreMonad k = do
  runStderrLoggingT $ runCore k
  

instance MonadBaseControl IO CoreMonad where
  type StM CoreMonad a = a
  liftBaseWith f =  liftIO $ f $ \m -> runCoreMonad m
  restoreM = return
