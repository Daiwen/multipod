{-# LANGUAGE OverloadedStrings #-}

module Import.PodcastReader
    ( getEpisodeTitle
    , getEpisodeUrl
    , getPodcastTitle
    , getPodcastEpisodes
    , ReaderError
    ) where

import Control.Monad.Catch
import Data.List
import Data.Text (Text, pack)
import Text.XML.Light.Types

data ReaderError =
    BadRSS String
    deriving (Eq, Show)

instance Exception ReaderError

getRSS :: (MonadThrow m) => [Content] -> m Element
getRSS cs =
    let element = foldl aux Nothing cs
    in case element of
           Nothing -> throwM $ BadRSS "Unable to find a 'rss' item in the DOM."
           Just e -> return e
  where
    aux acc c =
        case c of
            Elem e ->
                if qName (elName e) == "rss"
                    then Just e
                    else acc
            _ -> acc

getChannel :: (MonadThrow m) => [Content] -> m Element
getChannel cs =
    let element = foldl aux Nothing cs
    in case element of
           Nothing ->
               throwM $ BadRSS "Unable to find a 'channel' item in the DOM."
           Just e -> return e
  where
    aux acc c =
        case c of
            Elem e ->
                if qName (elName e) == "channel"
                    then Just e
                    else acc
            _ -> acc

getItems :: [Content] -> [Element]
getItems cs = foldl aux [] cs
  where
    aux acc c =
        case c of
            Elem e ->
                if qName (elName e) == "item"
                    then e : acc
                    else acc
            _ -> acc

getTitle :: (MonadThrow m) => [Content] -> m Text
getTitle cs =
    let element = foldl aux Nothing cs
    in case element of
           Nothing ->
               throwM $ BadRSS "Unable to find a 'title' item in the DOM."
           Just e -> return e
  where
    aux acc c =
        case c of
            Elem e ->
                if qName (elName e) == "title"
                    then let content = elContent e
                         in case content of
                                [Text d] -> Just $ pack $ cdData d
                                _ -> acc
                    else acc
            _ -> acc

getLinkEnclosure :: (MonadThrow m) => [Content] -> m Text
getLinkEnclosure cs =
    let element = foldl aux Nothing cs
    in case element of
           Nothing ->
               throwM $
               BadRSS
                   "Unable to find nested 'enclosure' and 'url' items in the DOM."
           Just e -> return e
  where
    aux acc c =
        case c of
            Elem e ->
                if qName (elName e) == "enclosure"
                    then let attributes = elAttribs e
                         in do url <-
                                   find
                                       (\a -> qName (attrKey a) == "url")
                                       attributes
                               return $ pack $ attrVal url
                    else acc
            _ -> acc

getPodcastEpisodes :: (MonadThrow m) => [Content] -> m [Element]
getPodcastEpisodes cs = do
    rss <- getRSS cs
    channel <- getChannel $ elContent rss
    let items = getItems $ elContent channel
    return items

getPodcastTitle :: (MonadThrow m) => [Content] -> m Text
getPodcastTitle cs = do
    rss <- getRSS cs
    channel <- getChannel $ elContent rss
    getTitle $ elContent channel

getEpisodeTitle :: (MonadThrow m) => Element -> m Text
getEpisodeTitle item = getTitle $ elContent item

getEpisodeUrl :: (MonadThrow m) => Element -> m Text
getEpisodeUrl item = getLinkEnclosure $ elContent item
