{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ReadEpisode where

import Import

import BasicPrelude hiding (map)

handleReadEpisode :: Handler ()
handleReadEpisode = do
    idEpM <- lookupGetParam "id"
    case idEpM of
      Just idEp -> runDB $ updateEpisodeIsRead True $ read idEp
      Nothing -> return ()


postReadEpisodeR :: Handler ()
postReadEpisodeR = do
    handleReadEpisode
    return ()
