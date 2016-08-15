{-# LANGUAGE OverloadedStrings #-}

module Multipod.PodcastData (
    CoreState, initState, getPodcasts, addPodcasts
  ) where

import Data.Either.Utils
import Data.ConfigFile
import Data.Text hiding (map)
import System.Environment
import System.Directory

type CoreState = ConfigParser

initState :: IO CoreState
initState = do
  home <- getHomeDirectory
  val <- readfile emptyCP $ home ++ "/.multipod"
  return $ forceEither val

getPodcasts :: CoreState -> [String]
getPodcasts cp = map snd $ forceEither $ items cp "podcasts"

addPodcasts :: CoreState -> String -> String -> Either CPError CoreState
addPodcasts cp title podcast = set cp "podcasts" title podcast
