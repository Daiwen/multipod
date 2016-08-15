{-# LANGUAGE OverloadedStrings #-}

module Multipod.PodcastReader (
  getEpisodeInfo, getPodcastTitle
  ) where

import Text.XML.Light.Input
import Text.XML.Light.Types
import Data.List hiding (append)
import Data.Text (Text, pack,append)

getRSS :: [Content] -> Maybe Element
getRSS cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "rss"
      then Just e
      else acc
    _ -> acc

getChannel :: [Content] -> Maybe Element
getChannel cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "channel"
      then Just e
      else acc
    _ -> acc

getItems :: [Content] -> [Element]
getItems cs = foldl aux [] cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "item"
      then e : acc
      else acc
    _ -> acc

getTitle :: [Content] -> Maybe Text
getTitle cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "title"
      then
        let content = elContent e in
        case content of
        [Text d] -> Just $ pack $ cdData d
        _ -> acc
      else acc
    _ -> acc

getLinkEnclosure :: [Content] -> Maybe Text
getLinkEnclosure cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "enclosure"
      then
        let attributes = elAttribs e in
        do
          url <- find (\a -> qName (attrKey a) == "url") attributes
          return $ pack $ attrVal url
      else acc
    _ -> acc

getEpisodeInfo :: [Content] -> Maybe [Text]
getEpisodeInfo cs = do
  rss <- getRSS cs
  channel <- getChannel $ elContent rss
  let items = getItems $ elContent channel
  sequence $ map
    (\item -> do
      t <- getTitle $ elContent item
      l <- getLinkEnclosure  $ elContent item
      return $ append t $ append " " l)
    items

getPodcastTitle :: [Content] -> Maybe Text
getPodcastTitle cs = do
  rss <- getRSS cs
  channel <- getChannel $ elContent rss
  getTitle $ elContent channel
