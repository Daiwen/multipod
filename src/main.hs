module Main where

import Control.Exception.Base
import Data.List
import System.Environment
import Network.HTTP

address = "http://www.bbc.co.uk/programmes/b00snr0w/episodes/downloads.rss"

--  getArgs >>= mapM putStrLn

main = do
  catch
    (simpleHTTP (getRequest address) >>= getResponseBody >>= putStrLn)
    (\x -> putStrLn $ show (x :: IOException)) 
