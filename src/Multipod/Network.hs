module Multipod.Network (
  requestBody
) where

import Network.HTTP

requestBody :: String -> IO String
requestBody address = simpleHTTP (getRequest address) >>= getResponseBody
