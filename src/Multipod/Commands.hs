{-# LANGUAGE OverloadedStrings #-}

module Multipod.Commands
  ( printEpisodes
  , add
  , unknownCommand
  , CommandError
  ) where

import Prelude hiding (putStrLn, unlines)

import System.REPL
import System.REPL.Types
import Data.Either
import Data.Text hiding (map, filter)
import Data.Text.IO
import Data.Maybe
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Network.URI (parseURI, uriScheme)
import Text.XML.Light.Input
import Text.XML.Light.Types

import Multipod.Network
import Multipod.PodcastData
import Multipod.PodcastReader
import Multipod.Core

data CommandError
  = NotUrl
  | NotHTTPS
  deriving (Eq)

instance Show CommandError where
  show NotUrl = "Argument is not a well formed URL."
  show NotHTTPS = "HTTPS not supported."

instance Exception CommandError

unknownCommand :: Command CoreMonad Text ()
unknownCommand =
  makeCommandN
    ""
    (const True)
    ""
    False
    []
    (repeat lineAsker)
    (\t _ -> liftIO $ putStrLn $ append "Unknown command: " t)

printEpisodes :: Command CoreMonad Text ()
printEpisodes =
  makeCommand
    "print_episodes"
    ("print_episodes" ==)
    "description"
    (\_ -> do
       podcasts' <- getPodcasts
       let podcasts = filter (isJust . parseURI) podcasts'
       episodes <-
         sequence $
         map
           (\address -> do
              htmlString <- requestBody address
              let contents = parseXML htmlString
              episodeInfos <- getEpisodeInfo contents
              return $ unlines episodeInfos)
           podcasts
       liftIO $ putStrLn $ unlines episodes)

urlAsker :: Asker' CoreMonad String
urlAsker =
  Asker
    "Enter a podcast address: "
    (Right . unpack)
    (\argument ->
        let uri = parseURI argument
        in case uri of
             Just url ->
               if ((toLower $ pack $ uriScheme url) == "https:")
                 then return $ Left $ SomeException NotHTTPS
                 else return $ Right argument
             Nothing -> return $ Left $ SomeException NotUrl)

add :: Command CoreMonad Text ()
add =
  makeCommand1
    "add"
    ("add" ==)
    "description"
    False
    urlAsker
    (\_ address -> do
       htmlString <- requestBody address
       let contents = parseXML htmlString
       title <- getPodcastTitle contents
       addPodcasts $ mkPodcast (unpack title) address
       liftIO $
         do putStrLn $ append title " added to the list of podcast."
       )
