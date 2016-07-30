{-# LANGUAGE OverloadedStrings #-}

module Multipod.Commands (
  print_episodes, add
  ) where

import Network.HTTP
import System.REPL
import Data.Text (unpack)
import Data.Maybe
import Control.Monad
import Text.XML.Light.Input
import Text.XML.Light.Types


import Multipod.Podcast_Data

print_episodes podcasts_state =
  makeCommand "print_episodes" ("print_episodes" ==) "description"
    (\t -> 
       msum $ map
         (\ (_, address) -> do
            htmlString <- simpleHTTP (getRequest address) >>= getResponseBody
            let contents = parseXML htmlString
            maybe (putStrLn "error")
              (\channel -> putStrLn $ unlines $ catMaybes $ map get_string $
                           get_items $ elContent channel)
              (do
                 rss <- get_rss contents
                 get_channel $ elContent rss))
         podcasts_state)


myasker :: Asker' IO String
myasker = Asker "Enter argument: " (Right . unpack) (return . Right)


add podcasts_state =
  makeCommand1
    "add" ("add" ==) "description" False myasker
       (\t a ->
--        find ConfigFile podcasts section
--        if is_valid
--        then add address to podcasts section
--        else print error message
          putStrLn a)
