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
import Control.Monad.State
import Data.Text

import Multipod.PodcastData
import Multipod.PodcastReader

type CoreState = ()

newtype CoreMonad a = CMo
  { runCore :: StateT CoreState (LoggingT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch,
              MonadState CoreState, MonadBase IO, MonadLogger)

runCoreMonad :: CoreMonad a -> CoreState -> IO a
runCoreMonad k state = do
  (res, _) <- runStderrLoggingT $ runStateT (runCore k) state
  return res

instance MonadBaseControl IO CoreMonad where
  type StM CoreMonad a = a
  liftBaseWith f =  liftIO $ f $ \m -> runCoreMonad m ()
  restoreM = return


initCoreState :: IO CoreState
initCoreState = return ()
