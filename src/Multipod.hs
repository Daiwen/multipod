{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Base
import Data.List
import System.Environment
import System.Directory
import System.REPL
import System.REPL.Prompt as PR
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.Commands
import Multipod.Core
import Multipod.Handlers

main = do
  state <- initCoreState
  runCoreMonad
    (makeREPL
       [add, printEpisodes]
       defExitCmd
       unknownCommand
       PR.prompt
       [networkHandler, readerHandler, dataHandler, commandHandler])
    state
