{-# LANGUAGE OverloadedStrings #-}

module Multipod.PodcastData (
    CoreState, initState, getPodcasts
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
