{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multipod.Core where

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Text

import Multipod.PodcastData

newtype CoreMonad a = CMo {
  runCore :: StateT CoreState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch,
            MonadState CoreState) 


runCoreMonad :: CoreMonad a -> CoreState -> IO a 
runCoreMonad k state =
  do
    (res, _) <- runStateT (runCore k) state
    return res
