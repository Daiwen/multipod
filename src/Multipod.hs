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

import Control.Exception.Base
import Control.Monad.Logger
import Data.List
import Data.Text               (Text)
import System.Environment
import System.Directory
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.App
import Multipod.Core
import Multipod.PodcastData

main = do
  initDataBase
  launchApp
      
