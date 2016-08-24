{-# LANGUAGE OverloadedStrings #-}

module Multipod.PodcastData (
    DataState, initDataState, saveState, getPodcasts, addPodcasts, DataError
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Either.Utils
import Data.ConfigFile
import Data.Text hiding (map)
import System.Environment
import System.Directory

type DataState = ConfigParser

data DataError = DataError CPError deriving (Eq, Show)

instance Exception DataError

getConfigFile :: IO String
getConfigFile = do
  home <- getHomeDirectory
  return $ home ++ "/.multipod"

saveState :: DataState -> IO ()
saveState cp = do
  file <- getConfigFile
  writeFile file $ to_string cp

initDataState :: (MonadIO m) => m DataState
initDataState = do
  config <- liftIO $ getConfigFile
  val <- liftIO $ readfile emptyCP config
  case val of
    Left  e -> return emptyCP
    Right t -> return t

getPodcasts :: (MonadThrow m) => DataState -> m [String]
getPodcasts cp =
  let episodes = items cp "podcasts" in
  case episodes of
    Left  e -> throwM $ DataError e
    Right r -> return $ map snd r

addPodcasts :: (MonadThrow m) => DataState -> String -> String -> m DataState
addPodcasts cp title podcast =
  case set cp "podcasts" title podcast of
    Left  e -> throwM $ DataError e
    Right t -> return t
