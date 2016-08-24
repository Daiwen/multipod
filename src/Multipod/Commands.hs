{-# LANGUAGE OverloadedStrings #-}

module Multipod.Commands (
  printEpisodes, add, unknownCommand
  ) where

import Prelude hiding (putStrLn, unlines)

import System.REPL
import Data.Either
import Data.Text hiding (map, filter)
import Data.Text.IO
import Data.Maybe
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Network.URI (parseURI)
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.Network
import Multipod.PodcastData
import Multipod.PodcastReader
import Multipod.Core


unknownCommand :: Command CoreMonad Text ()
unknownCommand =
  makeCommandN "" (const True) "" False [] (repeat lineAsker)
    (\t _ -> liftIO $ putStrLn $ append "Unknown command: " t)

printEpisodes :: Command CoreMonad Text ()
printEpisodes =
  makeCommand "print_episodes" ("print_episodes" ==) "description"
    (\_ -> do
       state <- get
       podcasts' <- getPodcasts state
       let podcasts  = filter (isJust . parseURI) podcasts'
       episodes <- sequence $ map
         (\address -> do
            htmlString <- requestBody address
            let contents = parseXML htmlString
            episodeInfos <- getEpisodeInfo contents
            return $ unlines episodeInfos)
         podcasts
       liftIO $ putStrLn $ unlines episodes)


hiddenAsker :: Asker' CoreMonad String
hiddenAsker  = Asker "Enter argument: " (Right . unpack) (return . Right)

add :: Command CoreMonad Text ()
add =
  makeCommand1
    "add" ("add" ==) "description" False hiddenAsker
    (\_ address -> do
       state <- get
       htmlString <- requestBody address
       let contents = parseXML htmlString
       title <- getPodcastTitle contents
       newState <- addPodcasts state (unpack title) address

       liftIO $ do
         putStrLn $ append title " added to the list of podcast."
         saveState newState
       put newState)
