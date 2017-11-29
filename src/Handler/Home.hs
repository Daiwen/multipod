{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import


displayHome :: [Entity Podcast] -> Handler Html
displayHome podcasts =
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
        setTitle "Synced podcasts"
        $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = do
    podcasts <- runDB getAllPodcast
    displayHome podcasts
