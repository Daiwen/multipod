{-# LANGUAGE OverloadedStrings #-}

module Multipod.Commands (
  print_episodes
  ) where

import Prelude hiding (putStrLn, unlines)
import Network.HTTP
import System.REPL
import Data.Text hiding (map)
import Data.Text.IO
import Data.Maybe
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.PodcastData
import Multipod.PodcastReader
import Multipod.Core


print_episodes :: Command CoreMonad Text ()
print_episodes =
  makeCommand "print_episodes" ("print_episodes" ==) "description"
    (\_ -> do
       state <- get
       let podcasts = getPodcasts state
       episodes <- (liftIO $ sequence $ map
         (\address -> do
            htmlString <- simpleHTTP (getRequest address) >>= getResponseBody
            let contents = parseXML htmlString
                resultString =
                  maybe ("error")
                    (\channel -> unlines $ catMaybes $ map get_string $
                                 get_items $ elContent channel)
                    (do
                       rss <- get_rss contents
                       get_channel $ elContent rss)
            return resultString)
         podcasts)
       liftIO $ putStrLn $ unlines episodes)
