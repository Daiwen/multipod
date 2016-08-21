{-# LANGUAGE OverloadedStrings #-}

module Multipod.Commands (
  printEpisodes, add
  ) where

import Prelude hiding (putStrLn, unlines)
import System.REPL
import Data.Text hiding (map)
import Data.Text.IO
import Data.Maybe
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.Network
import Multipod.PodcastData
import Multipod.PodcastReader
import Multipod.Core
import Data.Either

--TODO get rid of that !
import Data.ConfigFile hiding (get)


printEpisodes :: Command CoreMonad Text ()
printEpisodes =
  makeCommand "print_episodes" ("print_episodes" ==) "description"
    (\_ -> do
       state <- get
       let podcasts = getPodcasts state
       episodes <- (liftIO $ sequence $ map
         (\address -> do
            htmlString <- requestBody address
            let contents = parseXML htmlString
                episodeInfos =
                  maybe ("error") unlines (getEpisodeInfo contents)
            return episodeInfos)
         podcasts)
       liftIO $ putStrLn $ unlines episodes)


hiddenAsker :: Asker' CoreMonad String
hiddenAsker  = Asker "Enter argument: " (Right . unpack) (return . Right)

add :: Command CoreMonad Text ()
add =
  makeCommand1
    "add" ("add" ==) "description" False hiddenAsker
    (\_ address -> do
       state <- get

       --TODO catch exceptions here so that it doesn't crach everything
       htmlString <- liftIO $ requestBody address
       let contents = parseXML htmlString
           state' =
             maybe (Left (OtherProblem "Invalid address.", ""))
               (\title -> do
                  state' <- addPodcasts state (unpack title) address
                  return (title, state'))
               (getPodcastTitle contents)
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
