{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RemovePodcast where

import Import

handleRemovePodcast :: Handler ()
handleRemovePodcast = do
    nameM <- lookupGetParam "name"
    case nameM of
        Just name -> runDB $ removePodcast $ unpack name
        Nothing -> return ()

postRemovePodcastR :: Handler ()
postRemovePodcastR = do
    handleRemovePodcast
    return ()
