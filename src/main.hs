module Main where

import Control.Exception.Base
import Control.Monad
import Data.List
import Data.Either.Utils
import Data.Maybe
import Data.ConfigFile
import System.Environment
import System.Directory
import Network.HTTP
import Text.XML.Light.Input
import Text.XML.Light.Types

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


main = do
    home <- getHomeDirectory
    val <- readfile emptyCP $ home ++ "/.multipod"
    let cp = forceEither val
        podcasts = forceEither $ items cp "podcasts"
    
    msum $ map (\ (_, address) -> do
        htmlString <- simpleHTTP (getRequest address) >>= getResponseBody
        let contents = parseXML htmlString
        maybe (putStrLn "error")
          (\channel -> putStrLn $ unlines $ catMaybes $ map get_string $ get_items $ elContent channel)
          (do
             rss <- get_rss contents
             get_channel $ elContent rss))
      podcasts
