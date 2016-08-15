{-# LANGUAGE OverloadedStrings #-}

module Multipod.PodcastData (
    CoreState, initState, saveState, getPodcasts, addPodcasts
  ) where

import Data.Either.Utils
import Data.ConfigFile
import Data.Text hiding (map)
import System.Environment
import System.Directory

type CoreState = ConfigParser

getConfigFile :: IO String
getConfigFile = do
  home <- getHomeDirectory
  return $ home ++ "/.multipod"

saveState :: CoreState -> IO ()
saveState cp = do
  file <- getConfigFile
  writeFile file $ to_string cp

initState :: IO CoreState
initState = do
  config <- getConfigFile
  val <- readfile emptyCP config
  return $ forceEither val

getPodcasts :: CoreState -> [String]
getPodcasts cp = map snd $ forceEither $ items cp "podcasts"

addPodcasts :: CoreState -> String -> String -> Either CPError CoreState
addPodcasts cp title podcast = set cp "podcasts" title podcast
