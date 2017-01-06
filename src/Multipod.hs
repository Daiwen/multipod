{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Concurrent.Async
import Control.Exception.Base
import Control.Monad.Logger
import Data.List
import Data.Text               (Text)
import System.Environment
import System.Directory
import System.REPL
import System.REPL.Prompt as PR
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.App
import Multipod.Commands
import Multipod.Core
import Multipod.Handlers

main =
  race_ 
    launchApp
    $ runCoreMonad
      (makeREPL
         [add, printEpisodes]
         defExitCmd
         unknownCommand
         PR.prompt
         [networkHandler, readerHandler, dataHandler, commandHandler])
      
