{-# LANGUAGE OverloadedStrings #-}

module Multipod.Commands (
  print_episodes, add
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
import Data.Either

--TODO get rid of that !
import Data.ConfigFile hiding (get)


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


myasker :: Asker' CoreMonad String
myasker = Asker "Enter argument: " (Right . unpack) (return . Right)

add :: Command CoreMonad Text ()
add =
  makeCommand1
    "add" ("add" ==) "description" False myasker
    (\_ address -> do
       state <- get
       htmlString <-
         liftIO $ simpleHTTP (getRequest address) >>= getResponseBody
       let contents = parseXML htmlString
           state' =
             maybe (Left (OtherProblem "Invalid address.", ""))
               (\title -> do
                  state' <- addPodcasts state (unpack title) address
                  return (title, state'))
               (do
                  rss <- get_rss contents
                  channel <- get_channel $ elContent rss
                  get_title $ elContent channel)
       newState <- liftIO $
         case state' of
           Left (a, b) -> do
             putStrLn $ append (pack $ show a) $ append " at " (pack $ show b)
             return state
           Right (title, newState) -> do
             putStrLn $ append title " added to the list of podcast."
             saveState newState
             return newState
       put newState)
