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

import Multipod.PodcastData
import Multipod.Commands
import Multipod.Core

main = do
  state <- initState
  runCoreMonad (makeREPLSimple [print_episodes]) state
