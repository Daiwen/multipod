module Multipod.Podcast_Data (
  get_rss, get_channel, get_items, 
  get_title, get_link, get_string
  ) where

import Text.XML.Light.Input
import Text.XML.Light.Types
import Data.List

get_rss :: [Content] -> Maybe Element
get_rss cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "rss"
      then Just e
      else acc
    _ -> acc

get_channel :: [Content] -> Maybe Element
get_channel cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "channel"
      then Just e
      else acc
    _ -> acc

get_items :: [Content] -> [Element]
get_items cs = foldl aux [] cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "item"
      then e : acc
      else acc
    _ -> acc

get_title :: [Content] -> Maybe String
get_title cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "title"
      then
        let content = elContent e in
        case content of
        [Text d] -> Just $ cdData d
        _ -> acc
      else acc
    _ -> acc

get_link :: [Content] -> Maybe String
get_link cs = foldl aux Nothing cs
  where
  aux acc c = case c of
    Elem e ->
      if qName (elName e) == "enclosure"
      then
        let attributes = elAttribs e in
        do
          url <- find (\a -> qName (attrKey a) == "url") attributes
          return $ attrVal url
      else acc
    _ -> acc

get_string :: Element -> Maybe String
get_string item = do
  t <- get_title $ elContent item
  l <- get_link  $ elContent item
  return $ t ++ " " ++ l


