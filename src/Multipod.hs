{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Base
import Data.List
import Data.Either.Utils
import Data.ConfigFile
import System.Environment
import System.Directory
import System.REPL
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.Podcast_Data
import Multipod.Commands

main = do
    home <- getHomeDirectory
    val <- readfile emptyCP $ home ++ "/.multipod"
    let cp = forceEither val
        podcasts = forceEither $ items cp "podcasts"
    makeREPLSimple [print_episodes podcasts, add podcasts]
